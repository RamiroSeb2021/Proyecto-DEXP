#' Cálculo de réplicas según Harris–Hurvitz–Mood (HHM)
#'
#' Esta función estima el número de réplicas requerido usando el método HHM,
#' a partir de S1, d, df2 y K de la tabla A.9.
#'
#' @param S1 Estimación de la desviación estándar experimental.
#' @param d Diferencia mínima detectable entre medias.
#' @param df2 Grados de libertad del segundo estimador de la varianza.
#' @param K Valor de la tabla A.9 (según α).
#'
#' @return El número de réplicas (sin redondear).
#'
#' @examples
#' # Ejemplo 5.11
#' # S1 = sqrt(141.6), d = 20
#' r60 <- calcular_r_HHM(S1 = sqrt(141.6), d = 20, df2 = 60, K = 0.322)
#' print(r60)  # Esperado ~4.48
#' r25 <- calcular_r_HHM(S1 = sqrt(141.6), d = 20, df2 = 25, K = 0.502)
#' print(r25)  # Esperado ~4.64
#' 
#' @export
calcular_r_HHM <- function(S1, d, df2, K) {
  r <- 2 * (df2 + 1) * ((K * S1) / d)^2
  return(r)
}