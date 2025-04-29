test_that("hfr regression works", {
  library(dplyr)
  df_reg <- MASS::Boston
  colnames(df_reg)[1] <- "some non-syntactic name"
  df_cls3 <- iris
  df_cls2 <- iris |> filter(Species != "setosa")

  m_reg <- m("hfr", medv ~ ., df_reg, kappa = 0.4)
  expect_equal(nrow(coef(m_reg)), 14)
  expect_equal(nrow(predict(m_reg, df_reg)), 506)
  expect_equal(nrow(fitted(m_reg)), 506)
  expect_equal(nrow(resid(m_reg)), 506)
  expect_equal(nrow(predict(m_reg, df_reg |> select(-medv))), 506)

})
