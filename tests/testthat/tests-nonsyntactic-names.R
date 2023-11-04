test_that("regression with non-syntactic names", {
  library(dplyr)
  data <- data.frame(`Non-syntactic Name` = rnorm(100), normal.name = rnorm(100),
                     normal.factor = rep(c("a", "b"), 50), `Non-syntactic (factor)` = rep(c("a", "b", "c", "d"), each=25),
                     check.names = FALSE)
  fit <- regress(data,
                 `Non-syntactic Name` ~ normal.name + normal.factor + normal.name*`Non-syntactic (factor)` + `Non-syntactic (factor)`,
                 m("adalasso"),
                 m("bayes"),
                 m("blasso"),
                 m("bma"),
                 m("boost"),
                 m("bridge"),
                 m("cor"),
                 m("enet"),
                 m("genetic"),
                 m("gets"),
                 m("glm"),
                 m("hfr"),
                 m("lasso"),
                 m("lm"),
                 m("pcr"),
                 m("plsr"),
                 m("rf"),
                 m("quantile_rf", tau = c(0.1, 0.5)),
                 m("quantile", tau = c(0.1, 0.5)),
                 m("ridge"),
                 m("robust"),
                 m("spikeslab"),
                 m("subset", method = "forward", IC = "AIC"),
                 m("svm"),
                 m("tvp", niter = 50),
                 .cv="initial_split")
  coefs <- coef(fit)
  testthat::expect_false("errors" %in% colnames(fit))
  testthat::expect_false(any(is.na(coefs$term)))
})
