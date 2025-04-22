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
#' # Ejemplo 5.9
#' # t = 4 tratamientos, Δ = 3, σ² = 10.35, α = 0.05
#' res15 <- calcular_potencia(r = 15, t = 4, sigma2 = 10.35, Delta = 3)
#' print(res15)  # Esperado ~0.84
#' res4 <- calcular_potencia(r = 4, t = 4, sigma2 = 10.35, Delta = 3)
#' print(res4)   # Esperado ~0.79
#' 
#' @export
calcular_phi2 <- function(r, t, Delta, sigma2) {
  phi2 <- r * (Delta^2) / (2 * t * sigma2)
  return(phi2)
}

calcular_potencia <- function(r, t, sigma2, Delta, alpha = 0.05) {
  df1 <- t - 1   
  df2 <- t * (r - 1)
  Fcrit <- qf(1 - alpha, df1, df2)
  phi2 <- calcular_phi2(r, t, Delta, sigma2)
  power <- 1 - pf(Fcrit, df1, df2, ncp = phi2)
  return(list(r = r,
              phi2 = phi2,
              df1 = df1,
              df2 = df2,
              Fcrit = Fcrit,
              power = power))
}
