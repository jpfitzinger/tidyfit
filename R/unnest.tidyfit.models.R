
#' @importFrom tidyr unnest
#' @importFrom tibble new_tibble
#' @export
unnest.tidyfit.models <- function(data, cols, ...) {
  class(data) <- class(data)[-1]
  struc <- attr(data, "structure")
  df <- tidyr::unnest(data, {{cols}})
  df <- tibble::new_tibble(df, class = "tidyfit.models", structure = struc)
  return(df)
}
