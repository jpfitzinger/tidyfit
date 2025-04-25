
#' @importFrom tidyr unnest nest
#' @importFrom dplyr any_of select rename ungroup everything
#' @importFrom tibble new_tibble
#' @importFrom rsample int_pctl
#' @importFrom rlang .data

.make_rsample_bootstraps <- function(estimates, alpha = 0.05) {

  df_boot <- estimates |>
    dplyr::ungroup() |>
    tidyr::unnest(any_of("model_info")) |>
    dplyr::select(any_of(c("term", "estimate", "std.error", "slice_id"))) |>
    dplyr::rename(id = .data$slice_id) |>
    tidyr::nest(result = -.data$id)

  df_boot <- tibble::new_tibble(df_boot, class = c("rset", "bootstraps"))

  intervals <- rsample::int_pctl(df_boot, .data$result, alpha = alpha)
  intervals <- intervals |>
    dplyr::rename(estimate = .data$.estimate) |>
    dplyr::select(.data$term, .data$estimate, .data$.upper, .data$.lower)

  return(intervals)

}
