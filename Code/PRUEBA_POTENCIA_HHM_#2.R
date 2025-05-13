library(testthat)

# Pruebas para la función calcular_r_teorica
test_that("Prueba calcular_r_teorica", {
  # Caso de prueba estándar
  resultado <- calcular_r_teorica(t = 3, sigma2 = 4, Delta = 2)
  
  # Verificar que el resultado es una lista
  expect_type(resultado, "list")
  
  # Verificar que los elementos esperados están en el resultado
  expect_true("r" %in% names(resultado))
  expect_true("phi" %in% names(resultado))
  expect_true("phi2" %in% names(resultado))
  expect_true("df1" %in% names(resultado))
  expect_true("df2" %in% names(resultado))
  expect_true("potencia_objetivo" %in% names(resultado))
  
  # Verificar que el valor de r es mayor que 0
  expect_gt(resultado$r, 0)
  
  # Verificar que phi y phi2 son números positivos
  expect_gt(resultado$phi, 0)
  expect_gt(resultado$phi2, 0)
  
  # Verificar que df1 y df2 son enteros positivos
  expect_gt(resultado$df1, 0)
  expect_gt(resultado$df2, 0)
  
  # Probar validación para parámetros no válidos
  expect_error(calcular_r_teorica(t = -3, sigma2 = 4, Delta = 2), "Todos los parámetros deben ser > 0.")
  expect_error(calcular_r_teorica(t = 3, sigma2 = 4, Delta = 2, power_target = 0.85), "Potencia solicitada no tiene φ tabulado. Usa 0.80, 0.82 o 0.90.")
})

# Pruebas para la función calcular_r_HHM
test_that("Prueba calcular_r_HHM", {
  # Caso de prueba estándar
  resultado_hhm <- calcular_r_HHM(S2_1 = 5, d = 2, df2 = 30)
  
  # Verificar que el resultado es una lista
  expect_type(resultado_hhm, "list")
  
  # Verificar que los elementos esperados están en el resultado
  expect_true("S1" %in% names(resultado_hhm))
  expect_true("K" %in% names(resultado_hhm))
  expect_true("df2" %in% names(resultado_hhm))
  expect_true("r" %in% names(resultado_hhm))
  
  # Verificar que S1, K y r son números positivos
  expect_gt(resultado_hhm$S1, 0)
  expect_gt(resultado_hhm$K, 0)
  expect_gt(resultado_hhm$r, 0)
  
  # Probar validación para parámetros no válidos
  expect_error(calcular_r_HHM(S2_1 = -5, d = 2, df2 = 30), "Error en HHM: S2_1 debe ser un número > 0.")
  expect_error(calcular_r_HHM(S2_1 = 5, d = -2, df2 = 30), "Error en HHM: d debe ser un número > 0.")
  expect_error(calcular_r_HHM(S2_1 = 5, d = 2, df2 = 0), "Error en HHM: df2 debe ser un entero ≥ 1.")
  
  # Usar una expresión regular para permitir coincidencias más flexibles en el mensaje de error
  expect_error(calcular_r_HHM(S2_1 = 5, d = 2, df2 = 30, alpha = -0.05), 
               "Error en HHM: alpha debe estar en \\(0,1\\).")
})

