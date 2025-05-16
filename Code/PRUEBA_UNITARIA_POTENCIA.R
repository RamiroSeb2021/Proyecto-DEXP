library(testthat)
source("Code/Potencia.R")

test_that("calcular_r_teorica coincide con valores tabulados del libro (Tabla 5.13)", {
  # Caso 1
  res1 <- calcular_r_teorica(t = 4, sigma2 = 10.35, Delta = 3, power_target = 0.80)
  expect_equal(res1$r, 27)
  
  # Caso 2
  res2 <- calcular_r_teorica(t = 4, sigma2 = 10.35, Delta = 2, power_target = 0.80)
  expect_equal(res2$r, 61)
  
  # Caso 3
  res3 <- calcular_r_teorica(t = 4, sigma2 = 8.5, Delta = 3, power_target = 0.80)
  expect_equal(res3$r, 22, tolerance = 1)
  
  
  # Caso 4
  res4 <- calcular_r_teorica(t = 4, sigma2 = 10.35, Delta = 3, power_target = 0.90)
  expect_equal(res4$r, 35)
  
  # Caso 5
  res5 <- calcular_r_teorica(t = 4, sigma2 = 8.5, Delta = 4, power_target = 0.82)
  expect_equal(res5$r, 14)
})

