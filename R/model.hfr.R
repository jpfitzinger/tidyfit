#' @name .model.hfr
#' @title Hierarchical feature regression for \code{tidyfit}
#' @description Fits a hierarchical feature regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' - kappa (*proportional size of regression graph*)
#'
#' The hierarchical feature regression is estimated using the \code{hfr::cv.hfr} function. An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is provided (\code{is.null(control$kappa)}), the default is \code{seq(0, 1, by = 0.1)}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{hfr::cv.hfr}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Pfitzinger J (2022).
#' _hfr: Estimate Hierarchical Feature Regression Models_.
#' R package version 0.5.0, <https://CRAN.R-project.org/package=hfr>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("hfr", Return ~ ., data, kappa = 0.5)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("hfr", kappa = c(0.1, 0.5)), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{m}} method
#'
#' @importFrom dplyr mutate as_tibble select
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom methods formalArgs
#' @importFrom utils object.size

.model.hfr <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
    ) {

  f <- control$family
  names(control)[names(control)=="kappa"] <- "kappa_grid"
  control <- control[names(control) %in% methods::formalArgs(hfr::cv.hfr)]
  control$kappa_grid <- sort(control$kappa_grid)
  grid_ids <- formatC(seq_along(control$kappa_grid), 2, flag = "0")
  names(grid_ids) <- control$kappa_grid

  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, mf)
  y <- stats::model.response(mf)
  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  m <- do.call(hfr::cv.hfr, append(list(x = x, y = y, nfolds = 1, intercept = incl_intercept), control))

  model_handler <- purrr::partial(.handler.hfr, object = m, grid_ids = grid_ids,
                                  formula = formula, family = control$family)

  control <- control[!names(control) %in% c("weights")]
  settings <- .control_to_settings(control, "kappa_grid", grid_ids)

  out <- tibble(
    estimator = "hfr::hfr",
    size = utils::object.size(m),
    handler = list(model_handler),
    settings
  )

  return(out)

}

.handler.hfr <- function(object, data, formula = NULL, grid_ids = NULL, selected_id = NULL, ..., .what = "model") {

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
    mf <- stats::model.frame(formula, data)
    x <- stats::model.matrix(formula, mf)
    if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
    pred_mat <- sapply(object$kappa_grid, function(kap) stats::predict(object, x, kappa = kap))
    colnames(pred_mat) <- grid_ids
    pred <- pred_mat %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(truth = truth) %>%
      tidyr::pivot_longer(-any_of("truth"), names_to = "grid_id", values_to = "prediction")
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
    coefs <- stats::coef(object)
    estimates <- coefs %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(term = rownames(coefs)) %>%
      tidyr::pivot_longer(names_to = "grid_id", values_to = "estimate", -.data$term) %>%
      dplyr::mutate(grid_id = grid_ids[.data$grid_id])
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
