
.unnest_args <- function(row) {
  unnested_row <- row %>%
    dplyr::mutate(settings = .control_to_settings(row$model_object[[1]])$settings) %>%
    rename(grid_id_ = .data$grid_id) %>%
    unnest(settings) %>%
    dplyr::select(-any_of(c("weights")))
  if ("grid_id" %in% colnames(unnested_row)) {
    unnested_row <- unnested_row %>%
      dplyr::select(-grid_id_)
  } else {
    unnested_row <- unnested_row %>%
      dplyr::rename(grid_id = .data$grid_id_)
  }
  updated_row <- unnested_row %>%
    tidyr::nest(settings = any_of(names(row$model_object[[1]]$args)))
  return(updated_row)
}
