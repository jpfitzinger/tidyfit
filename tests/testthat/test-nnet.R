test_that("nnet regression works", {
  library(dplyr)
  df_reg <- MASS::Boston
  df_cls3 <- iris
  df_cls2 <- iris |> dplyr::filter(Species != "setosa")

  m_reg <- m("nnet", medv ~ ., df_reg, size = 15, decay = 0.1, maxit = 100)
  expect_equal(nrow(coef(m_reg)), 13)
  expect_equal(nrow(predict(m_reg, df_reg)), 506)
  expect_equal(nrow(fitted(m_reg)), 506)
  expect_equal(nrow(resid(m_reg)), 506)
  expect_equal(nrow(predict(m_reg, df_reg |> dplyr::select(-medv))), 506)

})

test_that("nnet classification works", {
  library(dplyr)
  df_cls3 <- iris
  df_cls2 <- iris |> dplyr::filter(Species != "setosa")

  m_reg <- classify(df_cls3, Species ~ ., m("nnet", size = 15, decay = 0.1, maxit = 100))
  expect_equal(nrow(coef(m_reg)), 12)
  expect_equal(nrow(predict(m_reg, df_cls3)), 450)
  # Should fitted return probabilities for each class or classification result (max prob only)? 
  expect_equal(nrow(fitted(m_reg)), 150)
  expect_equal(nrow(predict(m_reg, df_cls3 |> dplyr::select(-Species))), 450)
})

test_that("nnet classification (2class) works", {
  library(dplyr)
  df_cls3 <- iris
  df_cls2 <- iris |> dplyr::filter(Species != "virginica")
  df_cls2$Species <- droplevels(df_cls2$Species)

  m_reg <- classify(df_cls2, Species ~ ., m("nnet", size = 15, decay = 0.1, maxit = 100))
  expect_equal(nrow(coef(m_reg)), 8)
  # should 2-class prediction explicitly return positive class as well?
  expect_equal(nrow(predict(m_reg, df_cls2)), 100)
  # Should fitted return probabilities for each class or classification result (max prob only)? 
  expect_equal(nrow(fitted(m_reg)), 100)
  expect_equal(nrow(predict(m_reg, df_cls2 |> dplyr::select(-Species))), 100)

})
