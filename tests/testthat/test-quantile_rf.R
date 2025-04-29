test_that("quantile rf regression works", {
  library(dplyr)
  df_reg <- MASS::Boston
  colnames(df_reg)[1] <- "some non-syntactic name"

  m_reg <- m("quantile_rf", medv ~ ., df_reg, tau = 0.4)
  expect_equal(nrow(tidyfit::explain(m_reg, use_package = "randomForest")), 14)
  expect_equal(nrow(predict(m_reg, df_reg)), 506)
  expect_equal(nrow(fitted(m_reg)), 506)
  expect_equal(nrow(resid(m_reg)), 506)
  expect_equal(nrow(predict(m_reg, df_reg |> select(-medv))), 506)

})
