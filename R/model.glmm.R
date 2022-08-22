#' @name .model.glmm
#' @title Generalized linear mixed-effects model for \code{tidyfit}
#' @description Fits a linear or logistic mixed-effects model (GLMM) and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{lme4::glmer}.
#'
#' Note that the function should only be used in conjunction with \code{regress} or \code{classify} and not as a stand-alone function. This is because of the reliance on the formula syntax in \code{glmer}, which requires some specialized arguments passed to 'control'.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{glmer}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::mutate(data, Return = ifelse(Return > 0, 1, 0))
#' fit <- classify(data, Return ~ CMA + (CMA | Industry), logit = m("glmm"), .mask = "Date")
#' fit
#'
#'
#' @seealso \code{\link{.model.glm}} and \code{\link{m}} methods
#'
#' @importFrom lme4 glmer ranef fixef
#' @importFrom dplyr tibble bind_cols mutate all_of
#' @importFrom tidyr expand_grid pivot_longer
#' @importFrom purrr map_dfr
#' @importFrom methods formalArgs

.model.glmm <- function(x = NULL, y = NULL, control = NULL, ...) {

  idx <- control$index_col
  form <- control$model_formula
  control <- control[names(control) %in% methods::formalArgs(lme4::glmer)]

  re_mat <- purrr::map_dfr(idx, function(x) x)
  re_terms <- lme4::findbars(form)
  re_terms <- lapply(re_terms, deparse)
  dat <- data.frame(y = y, x, as.data.frame(re_mat), check.names = FALSE)
  clean_column_names <- sapply(colnames(x), function(nam) ifelse(nam != make.names(nam), paste0("`", nam, "`"), nam))
  form_adj <- paste(
    "y ~ ",
    paste(clean_column_names, collapse = " + "),
    " + ",
    paste(sapply(re_terms, function(i) paste0("(", i, ")")), collapse = " + ")
    )
  m <- do.call(lme4::glmer, append(list(formula = form_adj, data = dat), control))

  fe <- lme4::fixef(m)
  out_fe <- dplyr::tibble(
    variable = names(fe),
    beta = fe,
    family = list(control$family)
  )
  out <- tidyr::expand_grid(dplyr::distinct(re_mat), variable = out_fe$variable)
  out <- out %>%
    dplyr::left_join(out_fe, by = c("variable"))

  re <- lme4::ranef(m)
  for (i in seq_along(re)) {
    re_ <- re[[i]] %>%
      dplyr::as_tibble()
    re_[names(re)[i]] <- rownames(re[[i]])
    re_ <- re_ %>%
      tidyr::pivot_longer(-dplyr::all_of(names(re)[i]),
                          names_to = "variable",
                          values_to = "beta_temp")

    out <- out %>%
      dplyr::left_join(re_, by = c(names(re)[i], "variable")) %>%
      dplyr::mutate(beta = ifelse(!is.na(.data$beta_temp), .data$beta + .data$beta_temp, .data$beta)) %>%
      dplyr::select(-.data$beta_temp)
  }

  coefs <- stats::coef(m)
  for (i in seq_along(coefs)) {
    coefs_ <- coefs[[i]] %>%
      dplyr::as_tibble()
    coefs_[names(coefs)[i]] <- rownames(coefs[[i]])
    coefs_ <- coefs_ %>%
      tidyr::pivot_longer(-dplyr::all_of(names(coefs)[i]),
                          names_to = "variable",
                          values_to = paste0("beta_", names(coefs)[i]))

    out <- out %>%
      dplyr::left_join(coefs_, by = c(names(re)[i], "variable"))
  }

  control <- control[!names(control) %in% c("family")]
  if (length(control) > 0) {
    out <- dplyr::bind_cols(out, dplyr::as_tibble(.func_to_list(control)))
  }

  return(out)

}
