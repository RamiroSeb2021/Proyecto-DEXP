#' Cálculo del número de réplicas usando la teoría de potencia (φ²)
#'
#' @param t Número de tratamientos.
#' @param sigma2 Varianza estimada (σ²).
#' @param Delta Diferencia mínima detectable (Δ).
#' @param power_target Potencia deseada (1 - β), por defecto 0.80.
#' @param alpha Nivel de significancia (por defecto 0.05).
#' @param r_max Número máximo de réplicas a probar (por defecto 500).
#' @return Lista con r, φ, φ², df1, df2, potencia objetivo.
#' @export
calcular_r_teorica <- function(t, sigma2, Delta, power_target = 0.80, alpha = 0.05, r_max = 500) {
  if (any(c(t, sigma2, Delta) <= 0)) stop("Todos los parámetros deben ser > 0.")
  if (!(power_target %in% c(0.80, 0.82, 0.90))) stop("Potencia solicitada no tiene φ tabulado. Usa 0.80, 0.82 o 0.90.")
  
  # Constantes
  df1 <- t - 1
  phi_coef <- Delta^2 / (2 * t * sigma2)
  
  # Tabla de referencia (extraída de la teoría)
  tabla_phi2 <- data.frame(
    power = c(0.80, 0.82, 0.90),
    phi   = c(1.71, 1.81, 1.95)
  )
  
  phi  <- tabla_phi2$phi[tabla_phi2$power == power_target]
  phi2 <- phi^2
  r    <- phi2 / phi_coef
  r_final <- ceiling(r)
  df2 <- t * (r_final - 1)
  
  return(list(
    r = r_final,
    phi = phi,
    phi2 = phi2,
    df1 = df1,
    df2 = df2,
    potencia_objetivo = power_target
  ))
}

