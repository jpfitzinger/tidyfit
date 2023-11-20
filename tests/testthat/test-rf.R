test_that("glm regression works", {
  library(dplyr)
  df_reg <- MASS::Boston
  df_cls3 <- iris
  df_cls2 <- iris %>% filter(Species != "setosa")

  m_reg <- m("rf", medv ~ ., df_reg)
  expect_equal(nrow(coef(m_reg)), 14)
  expect_equal(nrow(predict(m_reg, df_reg)), 506)
  expect_equal(nrow(fitted(m_reg)), 506)
  expect_equal(nrow(resid(m_reg)), 506)
  expect_equal(nrow(predict(m_reg, df_reg %>% select(-medv))), 506)

})

test_that("glm classification works", {
  library(dplyr)
  df_cls3 <- iris
  df_cls2 <- iris %>% filter(Species != "virginica")

  m_reg <- classify(df_cls3, Species ~ ., m("rf"))
  expect_equal(nrow(coef(m_reg)), 15)
  expect_equal(nrow(predict(m_reg, df_cls2)), 300)
  expect_equal(nrow(fitted(m_reg)), 150)
  expect_equal(nrow(predict(m_reg, df_cls2 %>% select(-Species))), 300)

})
