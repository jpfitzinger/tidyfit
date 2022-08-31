#' @name .model.lasso
#' @title Lasso regression and classification for \code{tidyfit}
#' @description Fits a linear regression or classification with L1 penalty and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(L1 penalty)*
#'
#' The Lasso regression is estimated using \code{glmnet::glmnet} with \code{alpha = 1}. For classification pass \code{family = "binomial"} to \code{...} in \code{\link{m}} or use \code{\link{classify}}.
#'
#' If the response variable contains more than 2 classes, a multinomial response is used automatically.
#'
#' An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$lambda)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100. Note that the grid selection tools provided by \code{glmnet::glmnet} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{glmnet::glmnet}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
#' Regularization Paths for Generalized Linear Models via Coordinate Descent.
#' Journal of Statistical Software, 33(1), 1-22. URL https://www.jstatsoft.org/v33/i01/.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("lasso", Return ~ ., data, lambda = 0.5)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("lasso", lambda = c(0.1, 0.5)), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.enet}}, \code{\link{.model.ridge}}, \code{\link{.model.adalasso}} and \code{\link{m}} methods
#'
#' @importFrom dplyr mutate as_tibble select
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom methods formalArgs
#' @importFrom utils object.size

.model.lasso <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
) {

  control <- control[names(control) %in% methods::formalArgs(glmnet::glmnet)]
  control <- control[names(control) != "alpha"]
  control$lambda <- sort(control$lambda, TRUE)

  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, mf)
  y <- stats::model.response(mf)

  # Prepare 'family' arg
  if (is.null(control$family)) {
    control$family <- gaussian()
  }
  if (inherits(control$family, "character")) {
    f_name <- control$family
  } else {
    f_name <- control$family$family
  }
  if (!f_name %in% c("gaussian", "binomial", "multinomial"))
    stop("invalid 'family' argument")
  if (f_name == "gaussian") {
    control$family <- "gaussian"
    f <- gaussian()
  } else {
    control$family <- "multinomial"
    f <- binomial()
    y <- as.factor(y)
  }

  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 1, intercept = incl_intercept), control))

  control$lambda <- m$lambda
  grid_ids <- formatC(1:length(control$lambda), 2, flag = "0")

  model_handler <- purrr::partial(.handler.glmnet, object = m, grid_ids = grid_ids,
                                  formula = formula, family = control$family)

  control <- control[!names(control) %in% c("weights")]
  settings <- .control_to_settings(control, "lambda", grid_ids)

  out <- tibble(
    estimator = "glmnet::glmnet",
    size = utils::object.size(m),
    handler = list(model_handler),
    settings
  )

  return(out)

}

.handler.glmnet <- function(object,
                           data,
                           selected_id = NULL,
                           grid_ids = NULL,
                           formula = NULL,
                           family = NULL, ..., .what = "model") {

  if (.what == "model") {
    return(object)
  }

  if (.what == "predict") {
    response_var <- all.vars(formula)[1]
    if (response_var %in% colnames(data)) {
      truth <- data[, response_var]
    } else {
      data[, response_var] <- 1
      truth <- NULL
    }
    x <- stats::model.matrix(formula, data)
    if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
    pred_mat <- stats::predict(object, x, type = "response")
    dimnames(pred_mat)[[length(dim(pred_mat))]] <- grid_ids
    if (length(dim(pred_mat))==3) {
      class_vals <- dimnames(pred_mat)[[2]]
    } else {
      class_vals <- NULL
    }
    pred <- pred_mat %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(row_n = dplyr::row_number())
    if (!is.null(truth)) {
      pred <- dplyr::mutate(pred, truth = truth)
    }
    if (family == "multinomial") {
      pred <- pred %>%
        tidyr::pivot_longer(-dplyr::any_of(c("truth", "row_n")),
                            names_to = c("class", "grid_id"),
                            values_to = "prediction",
                            names_sep = "\\.")
    } else {
      pred <- pred %>%
        tidyr::gather("grid_id", "prediction", -dplyr::any_of(c("truth", "row_n")))
    }
    pred <- pred %>%
      dplyr::select(-.data$row_n)
    if (length(class_vals)==2) {
      pred <- pred %>%
        dplyr::filter(.data$class == sort(class_vals)[2]) %>%
        dplyr::select(-.data$class)
    }
    if (!is.null(selected_id)) {
      if (grepl("[.]", selected_id)) {
        pred <- pred %>%
          dplyr::filter(.data$grid_id == substring(selected_id, 6)) %>%
          dplyr::mutate(grid_id = selected_id)
      } else {
        pred <- pred %>%
          dplyr::mutate(grid_id = paste(substring(selected_id, 1, 4), .data$grid_id, sep = "."))
      }
    }
    return(pred)
  }

  if (.what == "estimates") {
    estimates <- broom::tidy(object)
    estimates <- estimates %>%
      dplyr::mutate(grid_id = grid_ids[.data$step]) %>%
      dplyr::select(-.data$step) %>%
      dplyr::mutate(term = ifelse(.data$term == "", "(Intercept)", .data$term))
    if ("class" %in% colnames(estimates)) {
      class_vals <- unique(estimates$class)
      if (length(class_vals) == 2) {
        estimates <- estimates %>%
          dplyr::mutate(estimate = .data$estimate * 2) %>%
          dplyr::filter(.data$class == sort(class_vals)[2]) %>%
          dplyr::select(-.data$class)
      }
    }
    if (!is.null(selected_id)) {
      if (grepl("[.]", selected_id)) {
        estimates <- estimates %>%
          dplyr::filter(.data$grid_id == substring(selected_id, 6)) %>%
          dplyr::select(-.data$grid_id)
      } else {
        estimates <- estimates %>%
          dplyr::mutate(grid_id = paste(substring(selected_id, 1, 4), .data$grid_id, sep = "."))
      }
    }
    return(estimates)
  }

}
