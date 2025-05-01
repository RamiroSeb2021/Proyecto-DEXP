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

## Excepciones Diana
handle_hhm_error_numero_de_tratamientos_positivo_mayor_que_cero <- function() {
  return("El número de tratamientos debe ser un número entero positivo mayor que cero.")
}

handle_hhm_error_numero_de_replicas_positivo_mayor_que_cero <- function() {
  return("El número de réplicas iniciales (r₀) debe ser un número entero positivo mayor que cero.")
}

handle_hhm_error_desviaciones_separas_por_coma <- function() {
  return("Debe ingresar las desviaciones estándar (σ) como una lista de números separados por comas.")
}

handle_hhm_error_desviaciones_mal_formateadas <- function() {
  return("Las desviaciones estándar (σ) contienen valores no numéricos o mal formateados. Por favor, revise la entrada.")
}

Excepciones_proporcionalidad_sin_costo <- function(a, r0, sigmas_str, show_error) {
  # Validar 'a': debe ser un entero positivo
  if (missing(a) || !is.numeric(a) || length(a) != 1 || is.na(a) || a < 1 || a != floor(a)) {
    show_error(handle_hhm_error_numero_de_tratamientos_positivo_mayor_que_cero())
    return(FALSE)
  }
  
  # Validar 'r0': debe ser un entero positivo
  if (missing(r0) || !is.numeric(r0) || length(r0) != 1 || is.na(r0) || r0 < 1 || r0 != floor(r0)) {
    show_error(handle_hhm_error_numero_de_replicas_positivo_mayor_que_cero())
    return(FALSE)
  }
  
  # Validar 'sigmas_str': debe ser cadena no vacía
  if (missing(sigmas_str) || !is.character(sigmas_str) || nchar(trimws(sigmas_str)) == 0) {
    show_error(handle_hhm_error_desviaciones_separas_por_coma())
    return(FALSE)
  }
  
  # Convertir cadena a vector numérico
  sigmas <- suppressWarnings(as.numeric(strsplit(sigmas_str, ",")[[1]]))
  
  # Validar que no haya valores no numéricos
  if (any(is.na(sigmas))) {
    show_error(handle_hhm_error_desviaciones_mal_formateadas())
    return(FALSE)
  }
  
  # Validar que la cantidad de sigmas coincida con 'a'
  if (length(sigmas) != a) {
    show_error(paste0("La cantidad de desviaciones estándar (", length(sigmas), 
                      ") debe ser igual al número de tratamientos (", a, ")."))
    return(FALSE)
  }
  
  return(sigmas)
}
