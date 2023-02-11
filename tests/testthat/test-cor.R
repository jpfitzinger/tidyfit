test_that("cor regression works", {
  library(dplyr)
  df_reg <- MASS::Boston
  df_cls3 <- iris
  df_cls2 <- iris %>% filter(Species != "setosa")

  m_reg <- m("cor", medv ~ ., df_reg)
  expect_equal(nrow(coef(m_reg)), 13)

})
