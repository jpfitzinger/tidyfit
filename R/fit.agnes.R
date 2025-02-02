#' @name .fit.agnes
#' @title Agglomerative nesting for \code{tidyfit}
#' @description Calculates clusters using agglomerative nesting on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{cluster}}.
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
#' * method
#' * metric
#'
#' The function provides a wrapper for \code{cluster::agnes}. See \code{?agnes} for more details.
#'
#' **Implementation**
#'
#' In addition to the arguments passed to \code{cluster::agnes}, the function takes the argument \code{k} which specifies the number of clusters. An argument \code{transpose} is also provided (default \code{FALSE}). When \code{transpose = TRUE}, the data is transposed and clustering is done on \code{cor(data)} instead.
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
#' fit <- m("agnes",  ~ SMB + HML, data)
#' fit
#'
#' # Within 'cluster' function
#' fit <- cluster(data, ~ ., m("agnes", k = 4), .mask = c("Date", "Industry"))
#' fitted(fit)
#'
#' # Cluster across columns
#' fit <- cluster(data, ~ ., m("agnes", k = 4, transpose = TRUE), .mask = c("Date", "Industry"))
#' fitted(fit)
#'
#' @seealso \code{\link{.fit.diana}}, \code{\link{.fit.dtw}}, \code{\link{.fit.dbscan}} and \code{\link{m}} methods
#'
#' @importFrom stats dist cutree cor

.fit.agnes <- function(
    self,
    data = NULL
) {
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  var_names <- colnames(x)
  self$set_args(k = 2, transpose = FALSE, metric = "euclidean", stand = TRUE, overwrite = FALSE)

  if (self$args$transpose) {
    x <- stats::dist(stats::cor(x), method = self$args$metric)
    self$set_args(diss = TRUE, overwrite = TRUE)
  } else {
    self$set_args(diss = FALSE, overwrite = TRUE)
  }

  ctr <- self$args[names(self$args) %in% methods::formalArgs(cluster::agnes)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(cluster::agnes, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = x), ctr))
  .store_on_self(self, res)
  self$fit_info <- list(var_names = var_names)
  invisible(self)
}

.fitted.agnes <- function(object, self, ...) {
  clusters <- stats::cutree(object, k = self$args$k)
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
