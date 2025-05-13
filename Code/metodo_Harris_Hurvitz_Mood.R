# Archivo: Code/metodo_Harris_Hurvitz_Mood.R

#' Interpola el valor K de la tabla A.9 para un α dado y grados de libertad df2
#' @param df2 Grados de libertad del segundo estimador de la varianza.
#' @param alpha Nivel de significancia (por defecto 0.05).
#' @return Valor K interpolado.
#' @export
obtener_K_A9 <- function(df2, alpha = 0.05) {
  tabla <- data.frame(
    df2 = c(25, 60, 120),
    K   = c(0.502, 0.322, 0.282)
  )
  if (df2 <= min(tabla$df2)) return(tabla$K[1])
  if (df2 >= max(tabla$df2)) return(tabla$K[nrow(tabla)])
  approx(tabla$df2, tabla$K, xout = df2)$y
}

#' Cálculo de réplicas según Harris–Hurvitz–Mood (HHM)
#'
#' @param S2_1 Varianza estimada (S2₁), debe ser > 0.
#' @param d Diferencia mínima detectable (en mismas unidades que S₁), debe ser > 0.
#' @param df2 Grados de libertad del segundo estimador (entero ≥ 1).
#' @param alpha Nivel de significancia (0 < α < 1).
#' @return Lista con:
#'   \item{S1}{Desviación estándar (redondeada a 2 decimales).}
#'   \item{K}{Valor interpolado de la tabla A.9.}
#'   \item{df2}{Grados de libertad df₂ usados.}
#'   \item{r}{Número de réplicas calculado según fórmula.}
#' @export
calcular_r_HHM <- function(S2_1, d, df2, alpha = 0.05) {
  # Validaciones de entrada
  if (!is.numeric(S2_1) || length(S2_1) != 1 || S2_1 <= 0) {
    stop("Error en HHM: S2_1 debe ser un número > 0.")
  }
  if (!is.numeric(d) || length(d) != 1 || d <= 0) {
    stop("Error en HHM: d debe ser un número > 0.")
  }
  if (!is.numeric(df2) || length(df2) != 1 || df2 < 1 || df2 %% 1 != 0) {
    stop("Error en HHM: df2 debe ser un entero ≥ 1.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("Error en HHM: alpha debe estar en (0,1).")
  }
  
  # 1) Estimar y redondear S1
  S1_raw <- sqrt(S2_1)
  S1      <- ceiling(S1_raw * 100) / 100
  
  # 2) Obtener K
  K_val <- obtener_K_A9(df2, alpha)
  
  # 3) Calcular r
  r_val <- 2 * (df2 + 1) * ((K_val * S1) / d)^2
  
  list(
    S1  = S1,
    K   = K_val,
    df2 = df2,
    r   = r_val
  )
}
