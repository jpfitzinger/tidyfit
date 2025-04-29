test_that("chisq classification works", {
  library(dplyr)
  df_cls3 <- iris
  df_cls2 <- iris |> filter(Species != "virginica")

  m_reg1 <- classify(df_cls2, Species ~ ., m("chisq"))
  m_reg2 <- classify(df_cls3, Species ~ ., m("chisq"))
  expect_equal(nrow(coef(m_reg1)), 4)
  expect_equal(nrow(coef(m_reg2)), 4)

})
