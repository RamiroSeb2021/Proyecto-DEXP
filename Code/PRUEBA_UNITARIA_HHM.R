library(testthat)
source("Code/metodo_Harris_Hurvitz_Mood.R")

test_that("obtener_K_A9 interpola correctamente", {
  expect_equal(obtener_K_A9(25, 0.05), 0.502)
  expect_equal(obtener_K_A9(60, 0.05), 0.322)
  expect_equal(obtener_K_A9(120, 0.05), 0.282)
  k_approx <- obtener_K_A9(80, 0.05)
  expect_true(k_approx < 0.322 && k_approx > 0.282)
})

test_that("calcular_r_HHM coincide con ejemplos del libro", {
  res60 <- calcular_r_HHM(S2_1 = 141.6, d = 20, df2 = 60, alpha = 0.05)
  expect_equal(res60$S1, ceiling(sqrt(141.6) * 100) / 100)
  expect_equal(res60$K, 0.322)
  expect_equal(round(res60$r, 2), 4.48)
  
  res25 <- calcular_r_HHM(S2_1 = 141.6, d = 20, df2 = 25, alpha = 0.05)
  expect_equal(res25$K, 0.502)
  expect_equal(round(res25$r, 2), 4.64)
})

test_that("calcular_r_HHM valida entradas inválidas", {
  expect_error(
    calcular_r_HHM(S2_1 = -1, d = 20, df2 = 60, alpha = 0.05),
    "S2_1 debe ser un número > 0"
  )
  expect_error(
    calcular_r_HHM(S2_1 = 141.6, d = 0, df2 = 60, alpha = 0.05),
    "d debe ser un número > 0"
  )
  expect_error(
    calcular_r_HHM(S2_1 = 141.6, d = 20, df2 = 0, alpha = 0.05),
    "df2 debe ser un entero ≥ 1"
  )
})
