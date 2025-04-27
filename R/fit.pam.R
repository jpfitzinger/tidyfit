#' @name .fit.pam
#' @title Partitioning Around Medoids for \code{tidyfit}
#' @description Calculates clusters using partitioning around medoids on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{cluster}}.
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
#' The function provides a wrapper for \code{cluster::pam}. See \code{?pam} for more details.
#'
#' **Implementation**
#'
#' In addition to the arguments passed to \code{cluster::pam}, the function takes the argument \code{k} which specifies the number of clusters. An argument \code{transpose} is also provided (default \code{FALSE}). When \code{transpose = TRUE}, the data is transposed and clustering is done on \code{cor(data)} instead.
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
#' fit <- m("pam",  ~ SMB + HML, data)
#' fit
#'
#' # Within 'cluster' function
#' fit <- cluster(data, ~ ., m("pam", k = 4), .mask = c("Date", "Industry"))
#' fitted(fit)
#'
#' # Cluster across columns
#' fit <- cluster(data, ~ ., m("pam", k = 4, transpose = TRUE), .mask = c("Date", "Industry"))
#' fitted(fit)
#'
#' @seealso \code{\link{.fit.diana}}, \code{\link{.fit.dtw}}, \code{\link{.fit.dbscan}} and \code{\link{m}} methods
#'
#' @importFrom stats dist cutree cor

.fit.pam <- function(
    self,
    data = NULL
) {
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  var_names <- colnames(x)
  
  # set default args
  self$set_args(transpose = FALSE, metric = "euclidean", stand = TRUE, 
                overwrite = FALSE)
  
  if (((NCOL(x) < 3) & self$args$transpose) | ((NROW(x) < 3) & !self$args$transpose))
    stop("must have at least 3 observations for clustering")
  
  if (self$args$stand) {
    x <- scale(x)
  }
  
  # 'x' always passed as distance matrix (due to transpose option)
  self$set_args(diss = TRUE, overwrite = TRUE)
  ctr <- self$args[names(self$args) %in% methods::formalArgs(cluster::pam)]
  ctr <- ctr[names(ctr) != "k"]
  
  eval_fun_ <- function(x, k, transpose, ...) {
    args <- list(...)
    if (transpose)
      x <- stats::cor(x)
    do.call(cluster::pam, append(list(x=x, k=k), args))
  }
  eval_fun <- do.call(purrr::partial, append(list(
    .f = eval_fun_, transpose = self$args$transpose), ctr))
  
  cluster_fun <- function(x, k) {
    clust_obj <- eval_fun(x = x, k = k)
    clusters <- clust_obj$clustering
    return(list(clustering=clusters, result = clust_obj))
  }
  
  d <- .get_cluster_dist_mat(x, self$args$transpose, self$args$metric)
  
  results <- .select_optimal_k(
    x, d, cluster_fun, eval_fun, k = self$args$k, k.min = self$args$k.min, 
    k.max = self$args$k.max, transpose = self$args$transpose, 
    method = self$args$nbclust_method)
  
  .store_on_self(self, results)
  self$fit_info <- list(var_names = var_names, optimal_k = results$result$k, 
                        clusters = results$result$clustering, 
                        coords = results$result$coords)
  invisible(self)
}

.fitted.pam <- function(object, self, ...) {
  if (self$args$transpose) {
    fitted_df <- tidyr::tibble(
      term = self$fit_info$var_names,
      fitted = self$fit_info$clusters
    ) |>
      dplyr::mutate(term = dplyr::if_else(.data$term %in% names(self$names_map), self$names_map[.data$term], .data$term))
  } else {
    fitted_df <- tidyr::tibble(
      fitted = self$fit_info$clusters
    )
  }
  fitted_df <- dplyr::bind_cols(fitted_df, self$fit_info$coords)
  
  return(fitted_df)
}
