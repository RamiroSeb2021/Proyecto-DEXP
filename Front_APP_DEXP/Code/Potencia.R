#' Cálculo teórico de r para alcanzar potencia objetivo
#'
#' Resuelve la ecuación Power(r) = power_target con uniroot, y devuelve
#' el menor entero ≥ solución continua.
#'
#' @param t Número de tratamientos.
#' @param sigma2 Varianza estimada (σ²).
#' @param Delta Diferencia mínima detectable.
#' @param alpha Nivel de significancia.
#' @param power_target Potencia deseada (1−β).
#' @param lower_r Límite inferior de búsqueda (por defecto 2).
#' @param upper_r Límite superior de búsqueda (por defecto 500).
#' @return Lista con r_continuo, r_entero, phi2, df1, df2 y potencia alcanzada.
#' @export
calcular_r_teorica <- function(t, sigma2, Delta,
                               alpha = 0.05, power_target = 0.8,
                               lower_r = 2, upper_r = 500) {
  
  # función interna: devuelve potencia para un r dado
  power_at_r <- function(r) {
    df1  <- t - 1
    df2  <- t * (r - 1)
    if (df1 < 1 || df2 < 1) return(-Inf)
    phi2 <- r * (Delta^2) / (2 * t * sigma2)
    Fcrit <- qf(1 - alpha, df1, df2)
    1 - pf(Fcrit, df1, df2, ncp = phi2)
  }
  
  # buscar la r continua que iguala power_target
  f <- function(r) power_at_r(r) - power_target
  root <- uniroot(f, lower = lower_r, upper = upper_r)$root
  
  # redondeo al entero mínimo que ya cumpla
  r_int <- ceiling(root)
  
  # recalcular con r_int
  df1   <- t - 1
  df2   <- t * (r_int - 1)
  phi2  <- r_int * (Delta^2) / (2 * t * sigma2)
  Fcrit <- qf(1 - alpha, df1, df2)
  power <- 1 - pf(Fcrit, df1, df2, ncp = phi2)
  
  list(
    r_continuo   = root,
    r            = r_int,
    phi2         = phi2,
    df1          = df1,
    df2          = df2,
    potencia     = power
  )
}
