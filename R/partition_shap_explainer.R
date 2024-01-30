# Partition explainer

partition_shap <- function(
  fitted_model_object,
  explainer_model_object
) {
  mod <- fitted_model_object
  mf <- stats::model.frame(mod$formula, mod$data)
  x <- stats::model.matrix(mod$formula, mf)
  x <- as.data.frame(x[, colnames(x)!="(Intercept)"])
  colnames(x) <- gsub("`", "", colnames(x))
  y <- stats::model.response(mf)
  fitted_values <- mod$predict(mf)$prediction
  k <- NCOL(x)
  r.squared <- .get_R2(y, fitted_values)
  var_names <- colnames(x)
  shap_values <- rep(1, k)
  names(shap_values) <- var_names
  shap_env <- new.env()
  shap_env$shap_values <- shap_values
  .partition_shap_branches(x, fitted_values, shap_env, explainer_model_object)
  return(shap_env$shap_values * r.squared)
}

.get_R2 <- function(y, fitted_values) {
  RSS <- sum((y - fitted_values)^2)
  TSS <- sum((y - mean(y))^2)
  R2 <- 1 - RSS/TSS
  return(R2)
}

.partition_shap_branches <- function(x, y, shap_env, explainer_model_object) {
  N <- NCOL(x)
  if (N == 1 || sd(y) == 0) {
    return(NULL)
  }
  if (N > 2) {
    dist <- as.dist(1 - abs(cor(x)))
    ord <- hclust(dist, method="single")$order
    clust_grps <- rep(2, N)
    clust_grps[ord[1:ceiling(N/2)]] <- 1
    grps <- paste0("G", clust_grps)
  } else {
    grps <- paste0("G", 1:N)
  }
  grps_list <- list()
  grp_names <- unique(grps)
  for (g in grp_names) {
    vec <- colnames(x)[grps==g]
    grps_list[[g]] <- vec
  }
  if (explainer_model_object$model_object[[1]]$method %in% c("lm", "ridge")) {
    output <- .glmnet.shap_values(explainer_model_object, x, y, grps_list)
  } else if (explainer_model_object$model_object[[1]]$method %in% c("rf")) {
    output <- .randomForest.shap_values(explainer_model_object, x, y, grps_list)
  } else {
    stop("only 'lm' or 'ridge' are currently permitted as explainer models")
  }
  for (grp in grp_names) {
    grp_cols <- grps_list[[grp]]
    shap_env$shap_values[grp_cols] <- shap_env$shap_values[grp_cols] * output$shap_values[grp]
    x_grp <- x[, grp_cols]
    y_grp <- output$fitted_values[[grp]]
    .partition_shap_branches(x_grp, y_grp, shap_env, explainer_model_object)
  }
}

.glmnet.shap_values <- function(
    explainer_model_object,
    x,
    y,
    groups
  ) {
  grp_names <- names(groups)
  obj <- explainer_model_object$model_object[[1]]$clone()

  # calculate full model
  mf <- data.frame(y = y, x)
  obj$formula <- as.formula("y ~ .")
  obj$fit(mf)
  fit <- obj$fitted()$fitted
  shap_values <- rep(.get_R2(y, fit)*0.5, 2)
  fitted_values <- list()
  for (g in grp_names) {
    x_i <- x
    g_other <- grp_names[grp_names != g]
    x_i[, groups[[g_other]]] <- 0
    fitted_values[[g]] <- obj$predict(x_i)$prediction * 0.5
  }

  # calculate group models
  for (i in seq_along(grp_names)) {
    g <- grp_names[i]
    i_other <- ifelse(i == 1, 2, 1)
    mf_i <- data.frame(y = y, x[, groups[[g]]])
    if (length(groups[[g]]) == 1) {
      mf_i$TEMP <- 1
    }
    obj$fit(mf_i)
    fit_i <- obj$fitted()$fitted
    R2_i <- .get_R2(y, fit_i)
    shap_values[i] <- shap_values[i] + R2_i * 0.5
    shap_values[i_other] <- shap_values[i_other] - R2_i * 0.5
    fitted_values[[g]] <- fitted_values[[g]] + fit_i*0.5
  }

  names(shap_values) <- grp_names
  shap_values <- shap_values + 1e-8
  shap_values <- shap_values / sum(shap_values)
  return(list(shap_values = shap_values, fitted_values = fitted_values))

}

.randomForest.shap_values <- function(
    explainer_model_object,
    x,
    y,
    groups
) {
  grp_names <- names(groups)
  obj <- explainer_model_object$model_object[[1]]$clone()

  # calculate full model
  mf <- data.frame(y = y, x)
  obj$formula <- as.formula("y ~ .")
  obj$fit(mf)
  fit <- obj$fitted()$fitted
  shap_values <- rep(.get_R2(y, fit)*0.5, 2)
  fitted_values <- list()
  for (g in grp_names) {
    x_i <- x
    g_other <- grp_names[grp_names != g]
    x_i[, groups[[g_other]]] <- 0
    fitted_values[[g]] <- obj$predict(data.frame(x_i))$prediction * 0.5
  }

  # calculate group models
  for (i in seq_along(grp_names)) {
    g <- grp_names[i]
    i_other <- ifelse(i == 1, 2, 1)
    mf_i <- data.frame(y = y, x[, groups[[g]]])
    obj$fit(mf_i)
    fit_i <- obj$predict(data.frame(mf_i))$prediction
    R2_i <- .get_R2(y, fit_i)
    shap_values[i] <- shap_values[i] + R2_i * 0.5
    shap_values[i_other] <- shap_values[i_other] - R2_i * 0.5
    fitted_values[[g]] <- fitted_values[[g]] + fit_i*0.5
  }

  names(shap_values) <- grp_names
  shap_values <- pmax(shap_values, 0) + 1e-8
  shap_values <- shap_values / sum(shap_values)
  return(list(shap_values = shap_values, fitted_values = fitted_values))

}
