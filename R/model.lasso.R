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
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
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
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("lasso", x, y)
#' fit
#'
#' @seealso \code{\link{.model.enet}}, \code{\link{.model.ridge}}, \code{\link{.model.adalasso}} and \code{\link{m}} methods
#'
#' @importFrom glmnet glmnet
#' @importFrom dplyr mutate as_tibble select
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.model.lasso <- function(
    x = NULL,
    y = NULL,
    control = NULL,
    ...
) {

  control <- control[names(control) %in% methods::formalArgs(glmnet::glmnet)]
  control <- control[names(control) != "alpha"]

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

  # Sparse coefficient matrix to tibble
  gather_coef_mat <- function(coef_mat) {
    colnames(coef_mat) <- formatC(1:ncol(coef_mat), 2, flag = "0")
    var_names <- rownames(coef_mat)
    var_names[1] <- "(Intercept)"
    out <- coef_mat %>%
      data.matrix %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(variable = var_names) %>%
      tidyr::gather("grid_id", "beta", -.data$variable) %>%
      dplyr::mutate(lambda = m$lambda[match(.data$grid_id, colnames(coef_mat))]) %>%
      dplyr::mutate(family = list(f)) %>%
      dplyr::select(.data$variable, .data$beta, .data$grid_id, .data$family, .data$lambda)
    return(out)
  }

  m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 1), control))

  coefs <- coef(m)

  if (f_name == "gaussian") {
    out <- gather_coef_mat(coefs)
  } else {
    lvls <- levels(y)
    out <- purrr::map_dfr(coefs, gather_coef_mat, .id = "class")
    if (length(lvls) == 2) {
      out <- out %>%
        dplyr::filter(.data$class == lvls[2]) %>%
        dplyr::select(-.data$class) %>%
        dplyr::mutate(beta = .data$beta * 2)
    }
  }

  control <- control[!names(control) %in% c("family", "lambda")]
  if (length(control) > 0) {
    out <- dplyr::bind_cols(out, as_tibble(func_to_list(control)))
  }

  return(out)

}
