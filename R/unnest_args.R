#' @importFrom dplyr mutate rename any_of select
#' @importFrom tidyr unnest nest

.unnest_args <- function(row, model_args = NULL) {
  if (missing(model_args)) model_args <- .args_to_frame(row$model_object[[1]])
  if (!is.null(row$model_object[[1]]$inner_grid)) {
    unnested_row <- row |>
      dplyr::select(-"grid_id") |>
      dplyr::bind_cols(row$model_object[[1]]$inner_grid)

  } else {
    unnested_row <- row
  }
  if (!is.null(model_args)) {
    model_args <- model_args |>
      dplyr::select(-any_of(colnames(model_args)[colnames(model_args) %in% colnames(unnested_row)]))
    unnested_row <- unnested_row |>
      dplyr::bind_cols(model_args)
  }
  updated_row <- unnested_row |>
    tidyr::nest(settings = any_of(names(row$model_object[[1]]$args)))
  return(updated_row)

}
