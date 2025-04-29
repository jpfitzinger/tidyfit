#' @name .fit.group_lasso
#' @title Grouped Lasso regression and classification for \code{tidyfit}
#' @description Fits a linear regression or classification with a grouped L1 penalty on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(L1 penalty)*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The Group Lasso regression is estimated using \code{gglasso::gglasso}. The 'group' argument is a named vector passed directly to `m()` (see examples). See \code{?gglasso} for more details. Only binomial classification is possible. Weights are ignored for classification.
#'
#' **Implementation**
#'
#' Features are standardized by default with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$lambda)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100. Note that the grid selection tools provided by \code{gglasso::gglasso} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @author Johann Pfitzinger
#' @references
#' Yang Y, Zou H, Bhatnagar S (2020). _gglasso: Group Lasso Penalized Learning Using a Unified BMD Algorithm_. R package version 1.5, <https://CRAN.R-project.org/package=gglasso>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' groups <- setNames(c(1, 2, 2, 3, 3, 1), c("Mkt-RF", "SMB", "HML", "RMW", "CMA", "RF"))
#'
#' # Stand-alone function
#' fit <- m("group_lasso", Return ~ ., data, lambda = 0.5, group = groups)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("group_lasso", lambda = c(0.1, 0.5), group = groups),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.lasso}}, \code{\link{.fit.blasso}}, \code{\link{.fit.adalasso}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.group_lasso <- function(
    self,
    data = NULL
) {

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- dplyr::select(as.data.frame(x), -"(Intercept)")
  standard_sd <- apply(x, 2, stats::sd)
  standard_mean <- apply(x, 2, mean)
  x <- data.matrix(scale(x, center = standard_mean, scale = standard_sd))

  # Prepare 'group' arg
  if (!is.null(self$args$group)) {
    # keep only relevant names
    groups <- self$args$group
    var_names <- names(groups) <- make.names(names(groups))
    var_names <- var_names[var_names %in% colnames(x)]
    group_vec <- groups[var_names]
    # add missing columns
    missing_names <- colnames(x)[!colnames(x) %in% var_names]
    group_vec <- c(group_vec, setNames(rep("_UNGROUPED", length(missing_names)), missing_names))
    # convert strings to integer
    int_map <- setNames(1:length(unique(group_vec)), unique(group_vec))
    group_vec_int <- int_map[group_vec]
    names(group_vec_int) <- names(group_vec)
    self$set_args(group_with_names = group_vec, overwrite = FALSE)
    self$set_args(group = group_vec_int, overwrite = TRUE)
  }

  # Prepare 'loss' arg
  if (self$mode == "regression") {
    if (!is.null(self$args$weights)) {
      self$set_args(loss = "wls", overwrite = FALSE)
    } else {
      self$set_args(loss = "ls", overwrite = FALSE)
    }
  }
  if (self$mode == "classification") {
    if (!is.null(self$args$weights)) {
      warning("group_lasso classification cannot handle weights, weights are ignored", call. = FALSE)
    }
    self$set_args(weights = NULL, overwrite = TRUE)
    self$set_args(loss = "logit", overwrite = FALSE)
    class_names_map <- levels(y)
    names(class_names_map) <- 1:length(levels(y))
    y <- ifelse(y == class_names_map[1], -1, 1)
  }
  # 'weights' must be passed as a matrix
  if (!is.null(self$args$weights)) {
    wts <- self$args$weights
    wts <- wts / sum(wts)
    wts <- diag(wts)
    self$set_args(weight = wts)
  }
  ctr <- self$args[names(self$args) %in% methods::formalArgs(gglasso::gglasso)]

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(gglasso::gglasso, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = x, y = y, intercept = incl_intercept), ctr))
  .store_on_self(self, res)
  if (!is.null(res$result$result$lambda))
    self$set_args(lambda = res$result$result$lambda)
  self$inner_grid <- data.frame(
    grid_id = paste(substring(self$grid_id, 1, 4), formatC(1:length(self$args$lambda), 2, flag = "0"), sep = "|"),
    lambda = self$args$lambda
  )

  fit_info = list(standard_sd = standard_sd, standard_mean = standard_mean)
  if (self$mode == "classification") {
    fit_info[["class_names_map"]] <- class_names_map
  }
  self$fit_info <- fit_info
  invisible(self)
}

.coef.gglasso <- function(object, self = NULL, ...) {
  estimates <- coef(object, s = self$args$lambda)
  var_names <- rownames(estimates)
  estimates <- apply(estimates, 2, function(x) {
    names(x) <- var_names
    return(.coef_rescaler(x, x_mean = self$fit_info$standard_mean, x_sd = self$fit_info$standard_sd))
  })
  colnames(estimates) <- self$inner_grid$grid_id[appr_in(self$inner_grid$lambda, self$args$lambda)]
  estimates <- dplyr::bind_cols(tidyr::tibble(term = rownames(estimates)), tidyr::as_tibble(estimates))
  estimates <- estimates |>
    tidyr::pivot_longer(-"term", names_to = "grid_id", values_to = "estimate") |>
    dplyr::left_join(self$inner_grid, by = "grid_id") |>
    dplyr::arrange(.data$grid_id, .data$term) |>
    dplyr::mutate(group = self$args$group_with_names[.data$term])
  return(estimates)
}

.predict.gglasso <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  x <- stats::model.matrix(self$formula, data)
  if ("(Intercept)" %in% colnames(x)) x <- dplyr::select(as.data.frame(x), -"(Intercept)")
  x <- data.matrix(scale(x, center = self$fit_info$standard_mean, scale = self$fit_info$standard_sd))
  pred_mat <- stats::predict(object, x, type = "link", s = self$args$lambda)
  if (self$mode == "classification") pred_mat <- binomial()$linkinv(pred_mat)

  dimnames(pred_mat)[[length(dim(pred_mat))]] <- self$inner_grid$grid_id[appr_in(self$inner_grid$lambda, self$args$lambda)]
  if (length(dim(pred_mat))==3) {
    class_vals <- dimnames(pred_mat)[[2]]
    dimnames(pred_mat)[[2]]  <- class_vals <- self$fit_info$class_names_map[class_vals]
  } else {
    class_vals <- NULL
  }
  pred <- pred_mat |>
    dplyr::as_tibble() |>
    dplyr::mutate(row_n = dplyr::row_number())
  if (!is.null(truth)) {
    pred <- dplyr::mutate(pred, truth = truth)
  }
  pred <- pred |>
    tidyr::gather("grid_id", "prediction", -dplyr::any_of(c("truth", "row_n")))
  pred <- pred |>
    dplyr::select(-"row_n")
  if (length(class_vals)==2) {
    pred <- pred |>
      dplyr::filter(.data$class == sort(class_vals)[2]) |>
      dplyr::select(-"class")
  }
  return(pred)
}

.fitted.gglasso <- function(object, self = NULL, ...) {
  fitted <- .predict.gglasso(object, .prepare_data(self, self$data), self) |>
    dplyr::rename(fitted = "prediction") |>
    dplyr::select(-dplyr::any_of(c("truth", "grid_id")))
  return(fitted)
}

.resid.gglasso <- function(object, self = NULL, ...) {
  residuals <- .predict.gglasso(object, self$data, self) |>
    dplyr::mutate(residual = .data$truth - .data$prediction) |>
    dplyr::select(-dplyr::any_of(c("truth", "prediction", "grid_id")))
  return(residuals)
}
