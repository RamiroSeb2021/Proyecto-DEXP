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
################################## 
## Excepciones Diana
handle_hhm_error_numero_de_tratamientos_positivo_mayor_que_cero <- function() {
  return("El número de tratamientos debe ser un número entero positivo mayor que cero. Por favor, revise los datos ingresados y consulte el ícono ⓘ para más información.")
}

handle_hhm_error_numero_de_replicas_positivo_mayor_que_cero <- function() {
  message <- "El número de réplicas iniciales debe ser un número entero positivo mayor que cero. Por favor, revise los datos ingresados y consulte el ícono ⓘ para más información."
  print(message)  # Para verificar que el mensaje se está generando
  return(message)
}

handle_hhm_error_desviaciones_separas_por_coma <- function() {
  return("Debes ingresar las desviaciones estándar como una lista de números separados por comas. Por favor, revise los datos ingresados y consulte el ícono ⓘ para más información.")}

handle_hhm_error_desviaciones_mal_formateadas <- function() {
  return("Las desviaciones estándar contienen valores no numéricos, negativos o con un formato incorrecto. Por favor, revise los datos ingresados y consulte el ícono ⓘ para más información.")
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

################################## 
handle_hhm_error_presupuesto_total_positivo <- function() {
  return("El presupuesto total debe ser un número positivo mayor que cero.Por favor, revise los datos ingresados y consulte el ícono ⓘ para más información.")
}

handle_hhm_error_cantidad_de_sigmas_distinta_a_tratamientos <- function(a) {
  return(paste0("Debe ingresar exactamente ", a, 
                " desviaciones estándar (σ), separadas por comas, una por cada tratamiento."))
}

handle_hhm_error_costos_separados_por_coma <- function() {
  return("Debe ingresar los costos como una lista de números separados por comas.")
}

handle_hhm_error_costos_mal_formateados <- function() {
  return("Los costos contienen valores no numéricos, negativos o mal formateados. Por favor, revise la entrada.")
}

handle_hhm_error_cantidad_de_costos_distinta_a_tratamientos <- function(a) {
  return(paste0("Debe ingresar exactamente ", a, 
                " costos, uno para cada tratamiento, separados por comas."))
}

Excepciones_proporcionalidad_con_costo <- function(a, sigmas_str, costos_str, costo_total, show_error) {
  # Validar 'a'
  if (missing(a) || !is.numeric(a) || length(a) != 1 || is.na(a) || a < 1 || a != floor(a)) {
    show_error(handle_hhm_error_numero_de_tratamientos_positivo_mayor_que_cero())
    return(FALSE)
  }
  
  # Validar 'costo_total'
  if (missing(costo_total) || !is.numeric(costo_total) || is.na(costo_total) || costo_total <= 0) {
    show_error(handle_hhm_error_presupuesto_total_positivo())
    return(FALSE)
  }
  
  # Validar sigmas_str
  if (missing(sigmas_str) || !is.character(sigmas_str) || nchar(trimws(sigmas_str)) == 0) {
    show_error(handle_hhm_error_desviaciones_separas_por_coma())
    return(FALSE)
  }
  sigmas_raw <- strsplit(sigmas_str, ",")[[1]]
  sigmas <- suppressWarnings(as.numeric(trimws(sigmas_raw)))
  if (any(is.na(sigmas)) || any(sigmas <= 0)) {
    show_error(handle_hhm_error_desviaciones_mal_formateadas())
    return(FALSE)
  }
  if (length(sigmas) != a) {
    show_error(handle_hhm_error_cantidad_de_sigmas_distinta_a_tratamientos(a))
    return(FALSE)
  }
  
  # Validar costos_str
  if (missing(costos_str) || !is.character(costos_str) || nchar(trimws(costos_str)) == 0) {
    show_error(handle_hhm_error_costos_separados_por_coma())
    return(FALSE)
  }
  costos_raw <- strsplit(costos_str, ",")[[1]]
  costos <- suppressWarnings(as.numeric(trimws(costos_raw)))
  
  # Validar si los costos son negativos o cero
  if (any(is.na(costos)) || any(costos <= 0)) {
    show_error(handle_hhm_error_costos_mal_formateados())
    return(FALSE)
  }
  
  if (length(costos) != a) {
    show_error(handle_hhm_error_cantidad_de_costos_distinta_a_tratamientos(a))
    return(FALSE)
  }
  
  # Verificación adicional antes de cálculos como sqrt (si es necesario)
  if (any(costos < 0)) {
    show_error("Los costos no pueden ser negativos.")
    return(FALSE)
  }
  
  # Todo válido, devolver lista
  return(list(sigmas))
}
