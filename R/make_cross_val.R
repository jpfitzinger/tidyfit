

.make_cross_val <- function(.data, .cv, .cv_args, gr_vars, .mask, .weights) {
  grps <- .data %>%
    dplyr::select(!!gr_vars) %>%
    dplyr::distinct()
  .data <- .data %>%
    dplyr::select(-!!gr_vars, -!!.mask)

  if (!is.null(.weights)) {
    wts <- .data %>%
      dplyr::pull(!!.weights)
    .data <- .data %>%
      dplyr::select(-dplyr::all_of(.weights))
  } else {
    wts <- NULL
  }

  mf <- data.frame(.data, check.names = FALSE)

  # Prepare CV
  if (!is.null(.cv_args$index)) {
    if (!.cv_args$index %in% colnames(mf))
      stop("'index_col' not found in the data")
  }

  cv_func <- switch(
    .cv,
    initial_split = rsample::initial_split,
    initial_time_split = rsample::initial_time_split,
    vfold = rsample::vfold_cv,
    loo = rsample::loo_cv,
    rolling_origin = rsample::rolling_origin,
    sliding_index = rsample::sliding_index,
    sliding_period = rsample::sliding_period,
    sliding_window = rsample::sliding_window,
    bootstraps = rsample::bootstraps
  )

  if (.cv == "none") {
    cv <- NULL
  } else {
    cv <- do.call(cv_func, append(list(data = mf), .cv_args))
    if (inherits(cv, "rsplit"))
      cv <- dplyr::tibble(splits = list(cv), id = .cv)
    if (!is.null(.cv_args$index)) {
      adj_id <- seq(1L, nrow(mf), by = ifelse(is.null(.cv_args$step), 1, .cv_args$step))
      adj_id <- adj_id[(length(adj_id) - nrow(cv) + 1):length(adj_id)]
      cv$id <- as.character(mf[adj_id, .cv_args$index])
    }
  }
  return(list(mf = mf, cv = cv, wts = wts, grps = grps))
}
