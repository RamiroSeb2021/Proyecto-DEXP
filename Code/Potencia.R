#' Cálculo de potencia para prueba F en ANOVA una vía
#'
#' Esta función calcula el parámetro de no centralidad φ² y la potencia (1−β)
#' de la prueba F en ANOVA una vía, dado r, t, Δ y σ².
#'
#' @param r Número de réplicas por tratamiento.
#' @param t Número de tratamientos.
#' @param Delta Diferencia mínima detectable entre medias.
#' @param sigma2 Varianza estimada (σ²).
#' @param alpha Nivel de significancia (por defecto 0.05).
#'
#' @return Una lista con:
#'   \item{r}{Número de réplicas.}
#'   \item{phi2}{Parámetro de no centralidad φ².}
#'   \item{df1}{Grados de libertad del numerador (t−1).}
#'   \item{df2}{Grados de libertad del denominador t(r−1).}
#'   \item{Fcrit}{Cuantil F(1−α; df1, df2).}
#'   \item{power}{Potencia de la prueba (1−β).}
#'
#' @examples
#' # Ejemplo básico
#' t <- 4; Delta <- 3; sigma2 <- 10.35
#' res15 <- calcular_potencia(r = 15, t = t, sigma2 = sigma2, Delta = Delta)
#' print(res15$power)  # ~0.84
#'
#' @export
calcular_phi2 <- function(r, t, Delta, sigma2) {
  phi2 <- r * (Delta^2) / (2 * t * sigma2)
  return(phi2)
}

#' Cálculo de potencia (1−β) para ANOVA una vía
#'
#' @inheritParams calcular_phi2
#' @param alpha Nivel de significancia (por defecto 0.05).
#'
#' @return Lista con r, phi2, df1, df2, Fcrit y power.
#'
#' @export
calcular_potencia <- function(r, t, sigma2, Delta, alpha = 0.05) {
  df1   <- t - 1
  df2   <- t * (r - 1)
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

#' Encontrar el número mínimo de réplicas para alcanzar potencia deseada
#'
#' Esta función usa un ciclo 'for' para iterar r desde un mínimo hasta un máximo,
#' calcula la potencia en cada paso y devuelve el primer r cuya potencia supera
#' el umbral especificado.
#'
#' @param t Número de tratamientos.
#' @param sigma2 Varianza estimada (σ²).
#' @param Delta Diferencia mínima detectable entre medias.
#' @param alpha Nivel de significancia (por defecto 0.05).
#' @param power_target Potencia mínima deseada (1−β), por defecto 0.80.
#' @param r_min Valor mínimo de r a considerar (por defecto 2).
#' @param r_max Valor máximo de r a considerar (por defecto 100).
#'
#' @return El valor de r (réplicas) cuya potencia es la primera ≥ power_target.
#'
#' @examples
#' # Ejemplo 5.9: t=4, Δ=3, σ²=10.35, potencia ≥0.80
#' res_r <- encontrar_r_minimo(t = 4, sigma2 = 10.35, Delta = 3,
#'                            alpha = 0.05, power_target = 0.80,
#'                            r_min = 2, r_max = 100)
#' print(res_r)  # Debe imprimir 27
#'
#' @export
encontrar_r_minimo <- function(t,
                               sigma2,
                               Delta,
                               alpha = 0.05,
                               power_target = 0.80,
                               r_min = 2,
                               r_max = 100) {
  r <- seq(r_min, r_max)
  potencia <- numeric(length(r))
  for (i in seq_along(r)) {
    potencia[i] <- calcular_potencia(r[i], t, sigma2, Delta, alpha)$power
  }
  # Selecciona el primer r que cumpla
  idx <- which(potencia >= power_target)[1]
  if (is.na(idx)) {
    stop(sprintf("No se encontró ningún r en [%d, %d] con potencia ≥ %.2f", r_min, r_max, power_target))
  }
  return(r[idx])
}
