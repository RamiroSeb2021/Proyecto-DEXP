# errores_potencia.R

handle_potencia_error_t <- function() {
  return("Error: t debe ser un entero ≥ 2.")
}

handle_potencia_error_sigma2 <- function() {
  return("Error: σ² debe ser positivo.")
}

handle_potencia_error_Delta <- function() {
  return("Error: Δ debe ser > 0.")
}

handle_potencia_error_alpha <- function() {
  return("Error: α debe estar en (0,1).")
}

handle_potencia_error_power <- function() {
  return("Error: potencia objetivo debe estar en (0,1).")
}

# errores_hhm.R

handle_hhm_error_S2 <- function() {
  return("Error: S2₁ debe ser > 0.")
}

handle_hhm_error_d <- function() {
  return("Error: d debe ser > 0.")
}

handle_hhm_error_df2 <- function() {
  return("Error: df₂ debe ser ≥ 1.")
}

handle_hhm_error_alpha <- function() {
  return("Error: α debe estar en (0,1).")
}

