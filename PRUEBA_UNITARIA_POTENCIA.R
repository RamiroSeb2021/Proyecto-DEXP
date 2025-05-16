library(testthat)

# Carga las funciones a testear
source("Code/Potencia.R")

test_that("calcular_phi2 coincide con ejemplo manual", {
  # Ejemplo: r = 27, t = 4, Δ = 3, σ² = 10.35
  phi2_manual <- 27 * 3^2 / (2 * 4 * 10.35)
  expect_equal(calcular_phi2(27, 4, 3, 10.35), phi2_manual)
})

test_that("calcular_potencia devuelve lista con campos correctos", {
  res <- calcular_potencia(r = 15, t = 4, sigma2 = 10.35, Delta = 3, alpha = 0.05)
  expect_type(res, "list")
  expect_named(res, c("r","phi2","df1","df2","Fcrit","power"))
  expect_equal(res$r, 15)
  expect_equal(res$df1, 3)
  expect_equal(res$df2, 4 * (15-1))
  expect_true(res$phi2 > 0)
  expect_true(res$Fcrit > 0)
  expect_true(res$power >= 0 && res$power <= 1)
})

test_that("encontrar_r_minimo da 27 para ejemplo del libro", {
  res <- encontrar_r_minimo(
    t           = 4,
    sigma2      = 10.35,
    Delta       = 3,
    alpha       = 0.05,
    power_target= 0.80,
    r_min       = 2,
    r_max       = 100
  )
  expect_equal(res$r, 27)
  # comprueba la potencia
  expect_true(res$power >= 0.80)
})

test_that("encontrar_r_minimo arroja error si no hay solución", {
  expect_error(
    encontrar_r_minimo(t = 4, sigma2 = 10.35, Delta = 3, alpha = 0.05, power_target = 0.99, r_max = 10),
    "No se encontró ningún r"
  )
})
