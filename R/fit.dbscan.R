#' @name .fit.dbscan
#' @title DBSCAN for \code{tidyfit}
#' @description Calculates clusters using density-based spatial clustering of applications with noise (DBSCAN) on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{cluster}}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' * k (number of clusters)
#' * eps
#' * minPts
#'
#' The function provides a wrapper for \code{dbscan::dbscan}. See \code{?dbscan} for more details.
#'
#' **Implementation**
#'
#' The formula can be specified with a LHS variable, as \code{~ x + z}.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns[1:100,]
#'
#' # Stand-alone function
#' fit <- m("dbscan",  ~ SMB + HML, data)
#' fit
#'
#' # Within 'cluster' function
#' fit <- cluster(data, ~ ., m("dbscan", eps = 1), .mask = c("Date", "Industry"))
#' fitted(fit)
#'
#' # Cluster across columns
#' fit <- cluster(data, ~ ., m("dbscan", eps = 1, transpose = TRUE), .mask = c("Date", "Industry"))
#' fitted(fit)
#'
#' @seealso \code{\link{.fit.diana}}, \code{\link{.fit.dtw}}, \code{\link{.fit.agnes}} and \code{\link{m}} methods
#'
#' @importFrom stats dist cutree cor

.fit.dbscan <- function(
    self,
    data = NULL
) {
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  var_names <- colnames(x)
  self$set_args(eps = 0.5, transpose = FALSE, stand = TRUE, metric = "euclidean", overwrite = FALSE)

  nvars <- NCOL(x)

  if (self$args$transpose) {
    x <- stats::dist(stats::cor(x), method = self$args$metric)
    self$set_args(minPts = min(nvars, 4), overwrite = FALSE)
  } else {
    if (self$args$stand) {
      x <- scale(x)
    }
    self$set_args(minPts = nvars + 1, overwrite = FALSE)
  }

  ctr <- self$args[names(self$args) %in% methods::formalArgs(dbscan::dbscan)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(dbscan::dbscan, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = x), ctr))
  .store_on_self(self, res)
  self$fit_info <- list(var_names = var_names)
  invisible(self)
}

.fitted.dbscan <- function(object, self, ...) {
  clusters <- object$cluster
  if (self$args$transpose) {
    fitted_df <- tidyr::tibble(
      term = self$fit_info$var_names,
      fitted = clusters
    ) |>
      dplyr::mutate(term = dplyr::if_else(.data$term %in% names(self$names_map), self$names_map[.data$term], .data$term))
  } else {
    fitted_df <- tidyr::tibble(
      fitted = clusters
    )
  }

  return(fitted_df)
}
