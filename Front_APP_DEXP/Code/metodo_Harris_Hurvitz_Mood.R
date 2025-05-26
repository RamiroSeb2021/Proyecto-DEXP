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
#' @param S2_1 Varianza estimada (S2₁).
#' @param d Diferencia mínima detectable (mismas unidades que S₁).
#' @param df2 Grados de libertad del segundo estimador.
#' @param alpha Nivel de significancia (por defecto 0.05).
#' @return Lista con S1 (redondeado), K, df2 y r.
#' @export
calcular_r_HHM <- function(S2_1, d, df2, alpha = 0.05) {
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


