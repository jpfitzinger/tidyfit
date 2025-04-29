test_that("group_lasso regression works", {
  library(dplyr)
  df_reg <- MASS::Boston
  colnames(df_reg)[1] <- "some non-syntactic name"
  df_cls3 <- iris
  df_cls2 <- iris |> filter(Species != "setosa")

  m_reg <- m("group_lasso", medv ~ ., df_reg, lambda = c(0.1))
  expect_equal(nrow(coef(m_reg)), 14)
  expect_equal(nrow(predict(m_reg, df_reg)), 506)
  expect_equal(nrow(fitted(m_reg)), 506)
  expect_equal(nrow(resid(m_reg)), 506)
  expect_equal(nrow(predict(m_reg, df_reg |> select(-medv))), 506)

})

test_that("group_lasso classification (2class) works", {
  library(dplyr)
  df_cls3 <- iris
  df_cls2 <- iris |> filter(Species != "virginica")

  m_reg <- classify(df_cls2, Species ~ ., m("group_lasso", lambda = c(0.01)))
  expect_equal(nrow(coef(m_reg)), 5)
  expect_equal(nrow(predict(m_reg, df_cls2)), 100)
  expect_equal(nrow(fitted(m_reg)), 100)
  expect_equal(nrow(predict(m_reg, df_cls2 |> select(-Species))), 100)

})
