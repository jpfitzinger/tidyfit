#' @importFrom dplyr pull select filter
#' @importFrom tidyr spread
#' @importFrom purrr map
#' @importFrom rlang .data


.fit_from_frame <- function(df, x) {

  f <- df %>%
    dplyr::pull(.data$family) %>%
    unique %>%
    .[[1]]

  if (!"class" %in% colnames(df)) {
    beta <- df %>%
      dplyr::select(.data$grid_id, .data$beta, .data$variable) %>%
      tidyr::spread(.data$grid_id, .data$beta)

    x <- x[, beta$variable]
    beta <- beta[, -1]
    fit <- x %*% data.matrix(beta)
    fit <- f$linkinv(fit)
    fit <- list(fit)
  } else {
    class_vals <- unique(df$class)
    fit_list <- class_vals %>%
      purrr::map(function(cls) {
        beta <- df %>%
          dplyr::filter(class == cls) %>%
          dplyr::select(.data$grid_id, .data$beta, .data$variable) %>%
          tidyr::spread(.data$grid_id, .data$beta)

        x <- x[, beta$variable]
        beta <- beta[, -1]
        fit <- exp(x %*% data.matrix(beta))
        return(fit)
      })
    fit_sum <- fit_list[[1]]
    for (i in 2:length(class_vals)) {
      fit_sum <- fit_sum + fit_list[[i]]
    }
    fit <- list()
    for (i in 1:length(class_vals)) {
      fit[[i]] <- fit_list[[i]] / fit_sum
    }
    names(fit) <- class_vals
  }

  return(fit)

}
