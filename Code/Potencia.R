# Archivo: Code/Potencia.R

#' Cálculo de φ² para prueba F en ANOVA una vía
#'
#' @param r Número de réplicas por tratamiento.
#' @param t Número de tratamientos.
#' @param Delta Diferencia mínima detectable entre medias.
#' @param sigma2 Varianza estimada (σ²).
#' @return φ² (parámetro de no centralidad).
#' @export
calcular_phi2 <- function(r, t, Delta, sigma2) {
  phi2 <- r * (Delta^2) / (2 * t * sigma2)
  return(phi2)
}

#' Cálculo de potencia (1−β) para ANOVA una vía
#'
#' @inheritParams calcular_phi2
#' @param alpha Nivel de significancia (por defecto 0.05).
#' @return Lista con: r, phi2, df1, df2, Fcrit y power.
#' @export
calcular_potencia <- function(r, t, sigma2, Delta, alpha = 0.05) {
  df1   <- t - 1
  df2   <- t * (r - 1)
  # Validar grados de libertad
  if (df1 < 1 || df2 < 1) {
    stop("Grados de libertad inválidos: revisa t y r.")
  }
  Fcrit <- qf(1 - alpha, df1, df2)
  phi2  <- calcular_phi2(r, t, Delta, sigma2)
  power <- 1 - pf(Fcrit, df1, df2, ncp = phi2)
  return(list(
    r      = r,
    phi2   = phi2,
    df1    = df1,
    df2    = df2,
    Fcrit  = Fcrit,
    power  = power
  ))
}

#' Encontrar el número mínimo de réplicas que alcance una potencia objetivo
#'
#' @param t Número de tratamientos.
#' @param sigma2 Varianza estimada (σ²).
#' @param Delta Diferencia mínima detectable entre medias.
#' @param alpha Nivel de significancia (por defecto 0.05).
#' @param power_target Potencia mínima deseada (1−β), por defecto 0.80.
#' @param r_min Réplicas mínimas a evaluar (por defecto 2).
#' @param r_max Réplicas máximas a evaluar (por defecto 500).
#' @return Lista con: r mínimo, phi2, df1, df2, Fcrit y power.
#' @export
encontrar_r_minimo <- function(t,
                               sigma2,
                               Delta,
                               alpha = 0.05,
                               power_target = 0.80,
                               r_min = 2,
                               r_max = 500) {
  for (r in seq(r_min, r_max)) {
    resultado <- calcular_potencia(r, t, sigma2, Delta, alpha)
    if (resultado$power >= power_target) {
      return(resultado)
    }
  }
  stop(sprintf("No se encontró ningún r en [%d, %d] con potencia ≥ %.2f",
               r_min, r_max, power_target))
}

