#' @name cross_prod
#' @title Prediction using a tidyfit data frame
#' @description This function performs a prediction using a tidyfit data frame
#'
#' @details Prediction is performed by multiplying the data frame with the coefficient vectors trained using \code{tidyfit}.
#'
#' @param fit a tibble of fitted models generated with \code{tidyfit}.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr). Must have the same structure as the input data to \code{tidyfit}.
#' @return A \code{tibble}.
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- tidyfit::Factor_Industry_Returns
#' fit <- regress(data, Return ~ ., lin_reg = m("lm"), .mask = "Date")
#' cross_prod(fit, data)
#'
#' @export
#'
#' @seealso \code{\link{regress}} and \code{\link{classify}} methods
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest nest all_of
#' @importFrom dplyr group_by across do
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
#' @importFrom utils globalVariables

utils::globalVariables(".")

cross_prod <- function(fit, data) {

  gr_vars <- group_vars(fit)
  if (!all(gr_vars %in% colnames(data)))
    stop("missing grouping variables in 'data'")

  data <- data %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(gr_vars)))

  df <- data %>%
    dplyr::do(result = .predict(., fit, gr_vars)) %>%
    tidyr::unnest(.data$result)

  df <- df %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(gr_vars)), .data$grid_id)

  return(df)

}
