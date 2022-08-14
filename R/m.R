#' @name m
#' @title Generic model wrapper for \code{tidyfit}
#' @description The function can fit various regression or classification models and returns the results as a tibble. \code{m()} can be used in conjunction with \code{\link{regress}} and \code{\link{classify}}, or as a stand-alone function.
#'
#' @details \code{model_method} specifies the model used to regress \code{y} on \code{x} and can take one of several options:
#'
#' ### Linear (generalized) regression or classification
#'
#' \code{"lm"} performs an OLS regression using \code{stats::lm}. See \code{\link{.model.lm}} for details.
#'
#' \code{"glm"} performs a generalized regression using \code{stats::glm}. See \code{\link{.model.glm}} for details.
#'
#' \code{"robust"} performs a robust regression using \code{MASS::rlm}. See \code{\link{.model.robust}} for details.
#'
#' \code{"quantile"} performs a quantile regression using \code{quantreg::rq}. See \code{\link{.model.quantile}} for details.
#'
#' ### Regression and classification with L1 and L2 penalties
#'
#' \code{"lasso"} performs a linear regression or classification with L1 penalty using \code{glmnet::glmnet}. See \code{\link{.model.lasso}} for details.
#'
#' \code{"ridge"} performs a linear regression or classification with L2 penalty using \code{glmnet::glmnet}. See \code{\link{.model.lasso}} for details.
#'
#' \code{"adalasso"} performs an Adaptive Lasso regression or classification using \code{glmnet::glmnet}. See \code{\link{.model.adalasso}} for details.
#'
#' \code{"enet"} performs a linear regression or classification with L1 and L2 penalties using \code{glmnet::glmnet}. See \code{\link{.model.enet}} for details.
#'
#' ### Gradient boosting
#'
#' \code{"boost"} performs gradient boosting regression or classificaiton using \code{mboost::glmboost}. See \code{\link{.model.boost}} for details.
#'
#' ### Factor regressions
#'
#' \code{"pcr"} performs a principal components regression using \code{pls::pcr}. See \code{\link{.model.pcr}} for details.
#'
#' \code{"plsr"} performs a partial least squares regression using \code{pls::plsr}. See \code{\link{.model.plsr}} for details.
#'
#' \code{"hfr"} performs a hierarchical feature regression using \code{hfr::hfr}. See \code{\link{.model.hfr}} for details.
#'
#' ### Best subset selection
#'
#' \code{"subset"} performs a best subset regression or classification using \code{bestglm::bestglm} (wrapper for \code{leaps}). See \code{\link{.model.subset}} for details.
#'
#' ### Bayesian regression
#'
#' \code{"bayes"} performs a Bayesian generalized regression or classification using \code{arm::bayesglm}. See \code{\link{.model.bayes}} for details.
#'
#' ### Miscellaneous
#'
#' \code{"cor"} calculates Pearson correlation coefficients using \code{stats::cor}. See \code{\link{.model.cor}} for details.
#'
#' When called without \code{x} and \code{y} arguments, the function returns a partialised version of itself that can be called with data to fit a model.
#'
#' @param model_method The name of the method to fit. See Details.
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param ...  Additional arguments passed to the underlying method function (e.g. \code{lm} or \code{glm}).
#' @param .remove_dependent_features When \code{TRUE}, linearly dependent features are removed using \code{qr(x)}. This avoids errors in several methods such as 'subsets' or´ 'lm'.
#' @param .return_method_name When \code{TRUE}, the function simply returns the 'method' argument.
#' @param .check_family When \code{TRUE}, the function returns a flag indicating whether a custom 'family' object has been passed to \code{...}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#'
#' @examples
#' # Stand-alone function
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rbinom(100, 1, 0.5)
#' fit <- m("glm", x, y)
#' fit
#'
#' # Within 'regress' function
#' data <- tidyfit::Factor_Industry_Returns
#' fit <- regress(data, Return ~ ., m("glm"), .mask = "Date")
#' fit
#'
#' @export
#'
#' @seealso \code{\link{regress}} and \code{\link{classify}} methods
#'
#' @importFrom purrr partial
#' @importFrom dials grid_regular penalty
#' @importFrom tidyr complete
#' @importFrom rlang .data dots_list
#' @importFrom dplyr mutate arrange relocate

m <- function(model_method,
              x = NULL,
              y = NULL,
              ...,
              .remove_dependent_features = FALSE,
              .return_method_name = FALSE,
              .check_family = FALSE
              ) {

  # Checks
  .check_method(model_method, "exists")
  additional_args <- list(...)
  if (.return_method_name) return(model_method)
  if (.check_family) return("family" %in% names(additional_args))

  # Partialised function when no data is passed
  if (is.null(x) & is.null(y)) {
    # args <- c(as.list(environment()), additional_args)
    args <- c(list(model_method = model_method, .remove_dependent_features = .remove_dependent_features),
              additional_args)
    args <- args[!names(args) %in% c("x", "y")]
    args <- append(args, list(.f = m))
    return(do.call(purrr::partial, args))
  }

  x <- data.frame(x, check.names = F)

  # Remove linearly dependent features
  if (.remove_dependent_features) {
    qr_x <- qr(x)
    x_ <- x[, qr_x$pivot[seq_len(qr_x$rank)]]
    if (ncol(x) > ncol(x_))
      warning("linearly dependent columns removed")
  } else {
    x_ <- x
  }

  # Set default hyperparameter grids
  default_grids <- .default_hp_grid(model_method,
                                    additional_args,
                                    nvars = ncol(x_))
  additional_args <- append(additional_args, default_grids)

  # Used to define the class
  tmp_ <- structure("", class = model_method)

  if (length(additional_args)==0) {
    args <- list(x = x_, y = y, control = additional_args, identifier = tmp_)
    mod <- do.call(.model, args)
    if ("grid_id" %in% colnames(mod)) {
      mod <- mod %>%
        tidyr::complete(variable = colnames(x), .data$grid_id, .data$family, fill = list(beta = 0))
    } else {
      mod <- mod %>%
        tidyr::complete(variable = colnames(x), .data$family, fill = list(beta = 0)) %>%
        dplyr::mutate(grid_id = "s000")
    }
  } else {
    args_grid <- .args_to_grid(model_method, additional_args)
    mod <- args_grid %>%
      purrr::map2_dfr(formatC(1:length(args_grid), 2, flag = "0"),
                      function(additional_args, grd_id) {
        args <- list(x = x_, y = y, control = additional_args, identifier = tmp_)
        mod <- do.call(.model, args)
        if ("grid_id" %in% colnames(mod)) {
          mod <- mod %>%
            tidyr::complete(variable = colnames(x), .data$grid_id, .data$family, fill = list(beta = 0)) %>%
            dplyr::mutate(grid_id = paste0(grd_id, .data$grid_id))
        } else {
          mod <- mod %>%
            tidyr::complete(variable = colnames(x), .data$family, fill = list(beta = 0)) %>%
            dplyr::mutate(grid_id = grd_id)
        }
      })

    # Sanitize grid_id labels
    grid_names <- sort(unique(mod$grid_id))
    grid_ids <- paste0("s", formatC(seq_along(grid_names), 3, flag = "0"))
    names(grid_ids) <- grid_names
    mod <- mod %>%
      dplyr::mutate(grid_id = grid_ids[.data$grid_id])
  }

  # Arrange output
  mod <- mod %>%
    dplyr::arrange(.data$grid_id, .data$variable) %>%
    dplyr::relocate(.data$variable, .data$beta, .data$grid_id, .data$family)

  return(mod)

}
