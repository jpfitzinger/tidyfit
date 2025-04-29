test_that("svm regression works", {
  library(dplyr)
  df_reg <- MASS::Boston
  colnames(df_reg)[1] <- "some non-syntactic name"
  df_cls3 <- iris
  df_cls2 <- iris |> filter(Species != "setosa")

  m_reg <- m("svm", medv ~ ., df_reg, cost = 1)
  expect_equal(nrow(coef(m_reg)), 14)
  expect_equal(nrow(predict(m_reg, df_reg)), 506)
  expect_equal(nrow(fitted(m_reg)), 506)
  expect_equal(nrow(resid(m_reg)), 506)
  expect_equal(nrow(predict(m_reg, select(df_reg, -"medv"))), 506)

})

test_that("svm classification works", {
  library(dplyr)
  df_cls3 <- iris
  df_cls2 <- iris |> filter(Species != "setosa")

  m_reg <- classify(df_cls3, Species ~ ., m("svm", cost = c(1)))
  expect_equal(nrow(predict(m_reg, df_cls3)), 450)
  expect_equal(nrow(fitted(m_reg)), 450)
  expect_equal(nrow(predict(m_reg, df_cls3 |> select(-"Species"))), 450)

})

test_that("svm classification (2class) works", {
  library(dplyr)
  df_cls3 <- iris
  df_cls2 <- iris |> filter(Species != "virginica")

  m_reg <- classify(df_cls2, Species ~ ., m("svm", cost = c(1)))
  expect_equal(nrow(coef(m_reg)), 5)
  expect_equal(nrow(predict(m_reg, df_cls2)), 100)
  expect_equal(nrow(fitted(m_reg)), 100)
  expect_equal(nrow(predict(m_reg, df_cls2 |> select(-"Species"))), 100)

})
