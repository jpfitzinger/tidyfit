

.predict <- function(.data, fit, gr_vars) {

  model_list <- unique(fit$model)

  # Evaluate methods
  result <-
    map_dfr(model_list, function(mod) {

      mask <- attr(fit, "structure")$mask
      family <- attr(fit, "family")
      if (!is.null(family)) {
        if (family == "binomial") f <- binomial()
        if (family == "gaussian") f <- gaussian()
        if (family == "poisson") f <- poisson()
      }

      .data_core <- .data %>%
        select(-!!mask, -!!gr_vars)

      fit_core <- fit %>%
        filter(model == mod) %>%
        right_join(.data %>% select(!!gr_vars), by = c(gr_vars)) %>%
        ungroup

      formula <- attr(fit, "formula")
      m <- model.frame(formula, data = .data_core)
      x <- model.matrix(formula, data = .data_core)
      y <- model.response(m)

      beta <- fit_core %>%
        select(variable, beta) %>%
        distinct %>%
        drop_na

      x <- x[, beta$variable]
      beta <- beta %>% select(-variable)
      fit <- data.matrix(x) %*% data.matrix(beta)
      if (!is.null(family)) fit <- f$linkinv(fit)

      result <- .data %>%
        mutate(pred = as.numeric(fit)) %>%
        select(-any_of(colnames(m[,-1])), -all_of(gr_vars)) %>%
        mutate(model = mod)

      return(result)

    })

  return(result)

}
