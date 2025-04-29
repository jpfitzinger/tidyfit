test_that("glmm regression works", {
  library(dplyr)
  df_reg <- tidyfit::Factor_Industry_Returns

  m_reg <- m("glmm", Return ~ `Mkt-RF` + `Mkt-RF` | Industry, df_reg)
  expect_equal(nrow(coef(m_reg)), 11)
  expect_equal(nrow(predict(m_reg, df_reg[1:10,])), 10)
  expect_equal(nrow(fitted(m_reg)), 7080)
  expect_equal(nrow(resid(m_reg)), 7080)
  expect_equal(nrow(predict(m_reg, df_reg |> select(-Return))), 7080)

})

test_that("glm classification works", {
  library(dplyr)
  df_reg <- tidyfit::Factor_Industry_Returns
  df_reg$Return <- ifelse(df_reg$Return > 0, 1, 0)

  m_reg <- classify(df_reg, Return ~ CMA + CMA | Industry, m("glmm"))
  expect_equal(nrow(coef(m_reg)), 11)
  expect_equal(nrow(predict(m_reg, df_reg[1:10,])), 10)
  expect_equal(nrow(fitted(m_reg)), 7080)
  expect_equal(nrow(predict(m_reg, df_reg |> select(-Return))), 7080)

})
