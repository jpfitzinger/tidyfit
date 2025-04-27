#' @importFrom utils getFromNamespace

.make_cross_val <- function(.data, .cv, .cv_args, gr_vars, .mask, .weights) {
  grps <- .data |>
    dplyr::select(!!gr_vars) |>
    dplyr::distinct()
  .data <- .data |>
    dplyr::select(-!!gr_vars)

  if (!is.null(.weights)) {
    wts <- .data |>
      dplyr::pull(!!.weights)
    .data <- .data |>
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
  if (!is.null(.cv_args$group)) {
    if (!.cv_args$group %in% colnames(mf))
      stop("'group' column not found in the data")
  }

  if (!.cv == "none") {
    # Check if cv method exists
    if (!exists(.cv, asNamespace("rsample"), mode = "function")) {
      stop(paste0("Method '", .cv, "' not exported by 'rsample'."))
    }
    cv_func <- utils::getFromNamespace(.cv, asNamespace("rsample"))
    cv <- do.call(cv_func, append(list(data = mf), .cv_args))
    if (inherits(cv, "rsplit"))
      cv <- dplyr::tibble(splits = list(cv), id = .cv)
    if (!is.null(.cv_args$index)) {
      adj_id <- seq(1L, nrow(mf), by = ifelse(is.null(.cv_args$step), 1, .cv_args$step))
      adj_id <- adj_id[(length(adj_id) - nrow(cv) + 1):length(adj_id)]
      cv$id <- as.character(mf[adj_id, .cv_args$index])
    }
    .mask = unique(append(.mask, c(.cv_args$index, .cv_args$group)))
  } else {
    cv <- NULL
  }

  return(list(mf = mf, cv = cv, wts = wts, grps = grps, mask = .mask))
}
