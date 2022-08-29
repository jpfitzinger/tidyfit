#' @name .model.ridge
#' @title Ridge regression and classification for \code{tidyfit}
#' @description Fits a linear regression or classification with L2 penalty and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(L2 penalty)*
#'
#' The ridge regression is estimated using \code{glmnet::glmnet} with \code{alpha = 0}. For classification pass \code{family = "binomial"} to \code{...} in \code{\link{m}} or use \code{\link{classify}}.
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
#' fit <- m("ridge", Return ~ ., data, lambda = 0.5)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("ridge", lambda = c(0.1, 0.5)), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.lasso}}, \code{\link{.model.adalasso}}, \code{\link{.model.enet}} and \code{\link{m}} methods
#'
#' @importFrom dplyr mutate as_tibble select
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom methods formalArgs
#' @importFrom utils object.size

.model.ridge <- function(formula = NULL,
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

  m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 0, intercept = incl_intercept), control))

  control$lambda <- m$lambda
  grid_ids <- formatC(1:length(control$lambda), 2, flag = "0")

  model_handler <- purrr::partial(.handler.glmnet, object = m, grid_ids = grid_ids,
                                  formula = formula, family = control$family)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control)) %>%
      dplyr::mutate(grid_id = grid_ids)
  } else {
    settings <- NULL
  }
  settings <- tidyr::nest(settings, settings = dplyr::everything())

  out <- tibble(
    estimator = "glmnet::glmnet",
    size = utils::object.size(m),
    handler = list(model_handler),
    settings
  )

  return(out)

}
