#' @importFrom dplyr pull mutate filter select tibble
#' @importFrom tidyr spread
#' @importFrom rlang .data
#' @importFrom purrr map_dfr

.eval_metrics <- function(result, x, y) {

  fit <- .fit_from_frame(result, x)

  f <- result %>%
    dplyr::pull(family) %>%
    unique %>%
    .[[1]]

  if (f$family == "binomial") {
    lvls <- levels(y)
  }

  if (length(fit)==1) {
    fit <- fit[[1]]
    if (f$family == "binomial") {
      # Calculate cross-entropy loss
      ix <- ifelse(y == lvls[2], 1, 0)
      crit <- apply(fit, 2, function(x) -mean(ix * log(x) + (1-ix) * log(1 - x)))
    } else if (f$family == "gaussian") {
      crit <- colMeans((y - fit)^2)
    }
  } else {
    class_vals <- names(fit)
    crit <- rep(0, ncol(fit[[1]]))
    names(crit) <- colnames(fit[[1]])
    for (i in 1:length(class_vals)) {
      ix <- ifelse(y == class_vals[i], 1, 0)
      crit_ <- apply(fit[[i]], 2, function(x) -mean(ix * log(x), na.rm = T))
      crit <- crit + crit_
    }
  }

  crit <- dplyr::tibble(grid_id = names(crit), crit = crit)

  return(crit)

}
