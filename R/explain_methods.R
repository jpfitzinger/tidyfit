.explain.default <- function(
    object,
    self,
    use_package = NULL,
    use_method = NULL,
    ...) {
  if (is.null(use_package)) {
    use_package = .get_default_explain_package(self)
    warning(sprintf("using explain package '%s'", use_package))
  }
  .check_explain_method(self, use_package)
  use_method <- .get_default_explain_method(use_package, use_method)
  explain_function <- paste(".explain", use_package, use_method, sep = "_")
  result_df = do.call(explain_function, append(list(self = self), list(...)))
  return (result_df)
}

.explain_sensitivity_lmg <- function(self, ...) {
  args <- list(...)
  args <- args[names(args) %in% methods::formalArgs(sensitivity::lmg)]
  args$logistic <- self$mode == "classification"
  mf <- stats::model.frame(self$formula, .prepare_data(self, self$data))
  x <- stats::model.matrix(self$formula, mf)
  if ("(Intercept)" %in% colnames(x)) intercept <- TRUE
  x <- as.data.frame(x[, colnames(x)!="(Intercept)"])
  y <- stats::model.response(mf)
  args[["X"]] <- x
  args[["y"]] <- y
  res <- do.call(sensitivity::lmg, args)
  result_df <- tibble(
    term = colnames(x),
    importance = res$lmg[, "original"]
  )
  return(result_df)
}

.explain_sensitivity_pmvd <- function(self, ...) {
  args <- list(...)
  args <- args[names(args) %in% methods::formalArgs(sensitivity::pmvd)]
  args$logistic <- self$mode == "classification"
  mf <- stats::model.frame(self$formula, .prepare_data(self, self$data))
  x <- stats::model.matrix(self$formula, mf)
  if ("(Intercept)" %in% colnames(x)) intercept <- TRUE
  x <- as.data.frame(x[, colnames(x)!="(Intercept)"])
  y <- stats::model.response(mf)
  args[["X"]] <- x
  args[["y"]] <- y
  res <- do.call(sensitivity::pmvd, args)
  result_df <- tibble(
    term = colnames(x),
    importance = res$pmvd[, "original"]
  )
  return(result_df)
}

.explain_sensitivity_johnson <- function(self, ...) {
  args <- list(...)
  args <- args[names(args) %in% methods::formalArgs(sensitivity::johnson)]
  args$logistic <- self$mode == "classification"
  mf <- stats::model.frame(self$formula, .prepare_data(self, self$data))
  x <- stats::model.matrix(self$formula, mf)
  if ("(Intercept)" %in% colnames(x)) intercept <- TRUE
  x <- as.data.frame(x[, colnames(x)!="(Intercept)"])
  y <- stats::model.response(mf)
  args[["X"]] <- x
  args[["y"]] <- y
  res <- do.call(sensitivity::johnson, args)
  result_df <- tibble(
    term = colnames(x),
    importance = res$johnson[, "original"]
  )
  return(result_df)
}

.explain_sensitivity_src <- function(self, ...) {
  args <- list(...)
  args <- args[names(args) %in% methods::formalArgs(sensitivity::src)]
  args$logistic <- self$mode == "classification"
  mf <- stats::model.frame(self$formula, .prepare_data(self, self$data))
  x <- stats::model.matrix(self$formula, mf)
  if ("(Intercept)" %in% colnames(x)) intercept <- TRUE
  x <- as.data.frame(x[, colnames(x)!="(Intercept)"])
  y <- stats::model.response(mf)
  args[["X"]] <- x
  args[["y"]] <- y
  res <- do.call(sensitivity::src, args)
  rank <- ifelse(!is.null(args$rank), args$rank, FALSE)
  res <- ifelse(rank, res$SRRC, res$SRC)[[1]]
  result_df <- tibble(
    term = colnames(x),
    importance = res
  )
  return(result_df)
}

.explain_sensitivity_pcc <- function(self, ...) {
  args <- list(...)
  args <- args[names(args) %in% methods::formalArgs(sensitivity::pcc)]
  args$logistic <- self$mode == "classification"
  mf <- stats::model.frame(self$formula, .prepare_data(self, self$data))
  x <- stats::model.matrix(self$formula, mf)
  if ("(Intercept)" %in% colnames(x)) intercept <- TRUE
  x <- as.data.frame(x[, colnames(x)!="(Intercept)"])
  y <- stats::model.response(mf)
  args[["X"]] <- x
  args[["y"]] <- y
  res <- do.call(sensitivity::pcc, args)
  rank <- ifelse(!is.null(args$rank), args$rank, FALSE)
  semi <- ifelse(!is.null(args$semi), args$semi, FALSE)
  if (rank) {
    res <- ifelse(semi, res$SPRCC, res$PRCC)
  } else {
    res <- ifelse(semi, res$SPCC, res$PCC)
  }
  res <- res[[1]]
  result_df <- tibble(
    term = colnames(x),
    importance = res
  )
  return(result_df)
}

.explain_iml_Shapley <- function(self, ...) {
  args <- list(...)
  data <- stats::model.frame(self$original_formula, self$data)
  data <- .prepare_data(self, data)
  predictor <- iml::Predictor$new(
    self,
    data = data[,-1],
    y = data[,1],
    predict.function = function(model, newdata) model$predict(newdata, check_cols=FALSE)$prediction)
  x0 <- data[1,-1]
  explainer <- do.call(iml::Shapley$new, append(list(predictor = predictor, x.interest=x0), args[names(args)%in%c("sample.size")]))
  samples <- 1:nrow(data)
  if (!is.null(args$which_rows)) samples <- args$which_rows[args$which_rows %in% samples]
  result_df <- map_dfr(samples, function(i) {
    explainer$explain(data[i,-1])
    return(explainer$results)
  })
  result_df <- result_df |>
    dplyr::as_tibble() |>
    dplyr::rename(term = .data$feature, importance = .data$phi)
  return(result_df)
}

.explain_iml_FeatureImp <- function(self, ...) {
  data <- stats::model.frame(self$original_formula, self$data)
  data <- .prepare_data(self, data)
  predictor <- iml::Predictor$new(
    self,
    data = data[,-1],
    y = data[,1],
    predict.function = function(model, newdata) model$predict(newdata, check_cols=FALSE)$prediction)
  loss <- ifelse(self$mode == "regression", "mae", "ce")
  explainer <- iml::FeatureImp$new(predictor, loss = loss)
  result_df <- dplyr::as_tibble(explainer$results) |>
    dplyr::rename(term = .data$feature)
  return(result_df)
}

.explain_iml_LocalModel <- function(self, ...) {
  args <- list(...)
  args <- args[names(args) %in% c("k", "gower.power", "kernel.width", "dist.fun")]
  data <- stats::model.frame(self$original_formula, self$data)
  data <- .prepare_data(self, data)
  predictor <- iml::Predictor$new(
    self,
    data = data[,-1],
    y = data[,1],
    predict.function = function(model, newdata) model$predict(newdata, check_cols = FALSE)$prediction)
  x0 <- data[1,-1]
  args[["predictor"]] <- predictor
  explainer <- do.call(iml::LocalModel$new, append(list(x.interest=x0), args))
  samples <- 1:nrow(data)
  if (!is.null(args$which_rows)) samples <- args$which_rows[args$which_rows %in% samples]
  result_df <- map_dfr(samples, function(i) {
    explainer$explain(data[i,-1])
    return(explainer$results)
  })
  result_df <- result_df |>
    dplyr::as_tibble() |>
    dplyr::rename(term = .data$feature, importance = .data$effect)
  return(result_df)
}

.explain_randomForest_mean_decrease_accuracy <- function(self, ...) {

  if (self$mode == "regression") {
    imp <- self$object$importance
    estimates <- dplyr::as_tibble(imp) %>%
      dplyr::mutate(term = rownames(imp)) %>%
      dplyr::mutate(importanceSD = self$object$importanceSD[.data$term]) %>%
      dplyr::rename(importance = "%IncMSE")
  } else {
    imp <- self$object$importance
    imp_MDacc <- imp[, -(ncol(imp)-1):-ncol(imp)]
    imp_Other <- imp[, (ncol(imp)-1):ncol(imp)]
    estimates <- dplyr::as_tibble(imp_MDacc) %>%
      dplyr::mutate(term = rownames(imp)) %>%
      tidyr::pivot_longer(-"term", names_to = "class", values_to = "Class_MeanDecreaseAccuracy")
    estimates_other <- dplyr::as_tibble(imp_Other) %>%
      dplyr::mutate(term = rownames(imp))
    estimates <- dplyr::left_join(estimates, estimates_other, by = "term")
    impSD <- self$object$importanceSD
    impSD_MDacc <- impSD[, -ncol(impSD)]
    impSD_Other <- impSD[, ncol(impSD)]
    estimatesSD <- dplyr::as_tibble(impSD_MDacc) %>%
      dplyr::mutate(term = rownames(impSD)) %>%
      tidyr::pivot_longer(-"term", names_to = "class", values_to = "Class_MeanDecreaseAccuracySD")
    estimatesSD_other <- dplyr::tibble(MeanDecreaseAccuracySD = impSD_Other) %>%
      dplyr::mutate(term = rownames(impSD))
    estimatesSD <- dplyr::left_join(estimatesSD, estimatesSD_other, by = "term")
    estimates <- estimates %>%
      dplyr::left_join(estimatesSD, by = c("term", "class"))
  }

  return(estimates)
}
