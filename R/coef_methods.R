# Generic methods to generate tidy coefficient frames

#' @importFrom stats coef quantile
#' @importFrom dplyr mutate select filter tibble as_tibble all_of
#' @importFrom tidyr pivot_longer
#' @importFrom broom tidy
#' @importFrom purrr quietly map_dbl

.coef.glmnet <- function(object, self = NULL, ...) {
  estimates <- broom::tidy(object)
  lambdaSel <- self$args$lambda
  estimates <- estimates %>%
    dplyr::mutate(grid_id = self$inner_grid[.data$step, "grid_id"]) %>%
    dplyr::select(-.data$step) %>%
    dplyr::mutate(term = ifelse(.data$term == "", "(Intercept)", .data$term)) %>%
    dplyr::filter(appr_in(.data$lambda, lambdaSel))
  if ("class" %in% colnames(estimates)) {
    class_vals <- unique(estimates$class)
    if (length(class_vals) == 2) {
      estimates <- estimates %>%
        dplyr::mutate(estimate = .data$estimate * 2) %>%
        dplyr::filter(.data$class == sort(class_vals)[2]) %>%
        dplyr::select(-.data$class)
    }
  }
  return(estimates)
}

.coef.lm <- function(object, self = NULL, ...) {
  if (is.null(self$args$vcov.)) {
    adj_obj <- object
  } else if (self$args$vcov. == "BS") {
    adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovBS(object))
  } else if (self$args$vcov. == "HAC") {
    adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovHAC(object))
  } else if (self$args$vcov. == "HC") {
    adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovHC(object))
  } else if (self$args$vcov. == "OPG") {
    adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovOPG(object))
  }
  estimates <- broom::tidy(adj_obj)
  if (!is.null(self$fit_info$names_map)) {
    estimates$term <- self$fit_info$names_map[estimates$term]
  }
  return(estimates)
}

.coef.glm <- function(object, self = NULL, ...) {
  estimates <- broom::tidy(object)
  if (!is.null(self$fit_info$names_map)) {
    estimates$term <- self$fit_info$names_map[estimates$term]
  }
  return(estimates)
}

.coef.rq <- function(object, ...) {
  estimates <- broom::tidy(object)
  return(estimates)
}

.coef.rlm <- function(object, self = NULL, ...) {
  if (is.null(self$args$vcov.)) {
    adj_obj <- object
  } else if (self$args$vcov. == "BS") {
    adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovBS(object))
  } else if (self$args$vcov. == "HAC") {
    adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovHAC(object))
  } else if (self$args$vcov. == "HC") {
    adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovHC(object))
  } else if (self$args$vcov. == "OPG") {
    adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovOPG(object))
  }
  estimates <- broom::tidy(adj_obj)
  return(estimates)
}

.coef.mvr <- function(object, self = NULL, ...) {
  beta <- drop(stats::coef(object, intercept = T, ncomp = self$args$ncomp))
  beta <- .coef_rescaler(beta, x_sd = self$fit_info$standard_sd)
  var_names <- names(beta)

  estimates <- dplyr::tibble(
    term = var_names,
    estimate = beta,
    ncomp = self$args$ncomp
  )

  if (!is.null(self$args$jackknife) & !is.null(self$args$validation)) {
    if (self$args$jackknife) {
      jack_test <- pls::jack.test(object, ncomp = self$args$ncomp)
      estimates <- estimates %>%
        dplyr::mutate(std.error = drop(jack_test$sd)[.data$term],
                      statistic = drop(jack_test$tvalues)[.data$term],
                      p.value = drop(jack_test$pvalues)[.data$term])
    }
  }

  return(estimates)
}

.coef.glmboost <- function(object, self = NULL, ...) {
  quiet_coefs <- purrr::quietly(stats::coef)
  beta <- purrr::map_dbl(self$fit_info$var_names,
                         function(x) quiet_coefs(object, which = x, off2int = F)$result[x])
  beta[1] <- beta[1] + object$offset
  names(beta) <- self$fit_info$var_names
  if (self$mode == "classification") {
    beta <- beta * 2
  }
  beta <- .coef_rescaler(beta, x_mean = self$fit_info$standard_mean,
                         x_sd = self$fit_info$standard_sd)

  estimates <- dplyr::tibble(
    term = self$fit_info$var_names,
    estimate = beta
  )
  return(estimates)
}

#' @importFrom rlang :=
#' @importFrom dplyr as_tibble mutate all_of
#' @importFrom tidyr pivot_longer
.coef.merMod <- function(object, ...) {
  coefs <- stats::coef(object)
  estimates <- coefs %>%
    purrr::map2_dfr(names(coefs), function(cf, nam) {
      coefs_ <- cf %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(!! nam := rownames(cf)) %>%
        tidyr::pivot_longer(-all_of(nam),
                            names_to = "term",
                            values_to = "estimate")
    })
  return(estimates)
}

.coef.shrinkTVP <- function(object, self = NULL, ...) {
  beta <- object$beta
  estimates <- purrr::map_dfr(beta, function(bet) {
    dplyr::tibble(
      estimate = apply(bet, 2, mean)[-1],
      upper = apply(bet, 2, stats::quantile, 0.95)[-1],
      lower = apply(bet, 2, stats::quantile, 0.05)[-1],
      posterior.sd = apply(bet, 2, sd)[-1],
      index = self$fit_info$index_var
    )
  }, .id = "term")
  estimates <- estimates %>%
    dplyr::mutate(term = gsub("beta_", "", .data$term)) %>%
    dplyr::mutate(term = ifelse(.data$term == "Intercept", "(Intercept)", .data$term))
  return(estimates)
}

.coef.MSM.lm <- function(object, self = NULL, ...) {
  beta <- data.matrix(object@Coef)
  beta_var <- data.matrix(object@seCoef^2)
  probs <- data.matrix(object@Fit@smoProb[-1,])
  beta_probs <- probs %*% beta
  beta_se_probs <- sqrt(probs %*% beta_var)
  estimates <- beta_probs %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(index = self$fit_info$index_var) %>%
    tidyr::gather("term", "estimate", -.data$index)
  estimates_se <- beta_se_probs %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(index = self$fit_info$index_var) %>%
    tidyr::gather("term", "std.error", -.data$index)
  estimates <- estimates %>%
    dplyr::left_join(estimates_se, by = c("term", "index"))
  colnames(probs) <- paste("Regime", 1:object@k, "Prob")
  probs_df <- dplyr::as_tibble(probs) %>%
    dplyr::mutate(index = self$fit_info$index_var)
  beta_df <- t(beta)
  colnames(beta_df) <- paste("Regime", 1:object@k, "Beta")
  beta_df <- dplyr::as_tibble(beta_df) %>%
    dplyr::mutate(term = rownames(beta_df))
  estimates <- estimates %>%
    dplyr::left_join(probs_df, by = "index") %>%
    dplyr::left_join(beta_df, by = "term")
  return(estimates)
}

.coef.cv.hfr <- function(object, self = NULL, ...) {

  coefs <- stats::coef(object)
  colnames(coefs) <- self$inner_grid$grid_id
  kappa_gridSel <- self$args$kappa_grid
  estimates <- coefs %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(term = rownames(coefs)) %>%
    tidyr::pivot_longer(names_to = "grid_id", values_to = "estimate", -.data$term) %>%
    dplyr::filter(appr_in(self$inner_grid[match(.data$grid_id, self$inner_grid$grid_id), "kappa_grid"], kappa_gridSel))

  return(estimates)

}

.coef.bma <- function(object, ...) {
  raw_estimates <- coef(object, include.constant = TRUE)
  estimates <- dplyr::tibble(
    term = rownames(raw_estimates),
    estimate = raw_estimates[, "Post Mean"],
    posterior_sd = raw_estimates[, "Post SD"],
    pip = raw_estimates[, "PIP"]
  )
  return(estimates)
}

.coef.svm <- function(object, self = NULL, ...) {
  if (self$args$kernel != "linear") {
    warning("No coefficients produced for 'svm' with nonlinear kernel.")
    return(NULL)
  }
  raw_estimates <- stats::coef(object)
  raw_estimates <- .coef_rescaler(raw_estimates,
                                  object$x.scale[[1]], object$x.scale[[2]],
                                  object$y.scale[[1]], object$y.scale[[2]])
  estimates <- dplyr::tibble(
    term = names(raw_estimates),
    estimate = raw_estimates
  )

  return(estimates)

}


