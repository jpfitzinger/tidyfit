test_that("boost regression works", {
  library(dplyr)
  df_reg <- MASS::Boston
  colnames(df_reg)[1] <- "some non-syntactic name"
  df_cls3 <- iris
  df_cls2 <- iris %>% filter(Species != "setosa")

  m_reg <- m("boost", medv ~ ., df_reg, mstop = 100, nu = 0.1)
  expect_equal(nrow(coef(m_reg)), 14)
  expect_equal(nrow(predict(m_reg, df_reg)), 506)
  expect_equal(nrow(fitted(m_reg)), 506)
  expect_equal(nrow(resid(m_reg)), 506)
  expect_equal(nrow(predict(m_reg, df_reg %>% select(-medv))), 506)

})

test_that("bayes classification works", {
  library(dplyr)
  df_cls3 <- iris
  df_cls2 <- iris %>% filter(Species != "virginica")
  df_cls2$Species <- factor(df_cls2$Species, levels = sort(unique(df_cls2$Species)))

  m_reg <- classify(df_cls2, Species ~ ., m("boost", mstop = 100, nu = 0.1))
  expect_equal(nrow(coef(m_reg)), 5)
  expect_equal(nrow(predict(m_reg, df_cls2)), 100)
  expect_equal(nrow(fitted(m_reg)), 100)
  expect_equal(nrow(predict(m_reg, df_cls2 %>% select(-Species))), 100)

})
