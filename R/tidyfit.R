#' @name tidyfit
#' @title Linear model estimation on tidy data
#' @description This function is a wrapper to fit many different types of linear
#' regression of classification model on a (potentially grouped) `tibble`.
#'
#' @details
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param ...  Additional arguments passed to \code{hclust}.
#' @return A \code{tibble}.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom magrittr `%>%`

tidyfit <- function(
  .data,
  formula,
  ...,
  .cv = c("none", "loo", "vfold", "ts"),
  .cv_args = list(v = 10),
  .hyper_grid = NULL,
  .weights = NULL,
  .mask = NULL,
  .return_slices = FALSE
) {

  # TODO: check names

  model_list <- list(...)
  .cv <- match.arg(.cv)
  if (is.null(.hyper_grid)) .hyper_grid <- list()

  gr_vars <- group_vars(.data)

  df <- .data %>%
    do(result = .fit(., formula, model_list, .cv, .cv_args,
                     .hyper_grid, .weights, gr_vars, .mask,
                     .return_slices)) %>%
    unnest(result)

  df <- df %>%
    group_by(model) %>%
    do(temp = select(., where(~!all(is.na(.)))))

  df <- df$temp %>%
    map_dfr(~nest(., model_info = -c(!!gr_vars, variable, beta, model)))

  df <- df %>%
    group_by(across(all_of(gr_vars)))

  attr(df, "formula") <- formula
  attr(df, "structure") <- list(mask = .mask, col_names = colnames(df))

  if (!is.null(.hyper_grid$family)) {
    attr(df, "family") <- .hyper_grid$family
  }

  return(df)

}
