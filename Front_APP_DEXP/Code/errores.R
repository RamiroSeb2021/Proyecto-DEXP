# Frase a concatenar ------------------------------------------------------
frase_pred <- "  Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."

# Mensajes de errores -----------------------------------------------------

# errores_potencia.R

handle_potencia_error_t <- function() {
  paste0("Los tratamientos deben de ser una cantidad entera mayor o igual a 2.", frase_pred)
}

handle_potencia_error_sigma2 <- function() {
  paste0("σ² debe ser positivo mayor a cero.", frase_pred)
}

handle_potencia_error_Delta <- function() {
  paste0("La diferncia mínima a detectar debe ser > 0.", frase_pred)
}

handle_potencia_error_alpha <- function() {
  paste0("α debe ser en (0,1).", frase_pred)
}

handle_potencia_error_power <- function() {
  paste0("La potencia objetivo debe ser en (0,1).", frase_pred)
}

# errores_hhm.R

handle_hhm_error_S2 <- function() {
  paste0("S2₁ debe ser > 0.", frase_pred)
}

handle_hhm_error_d <- function() {
  paste0("d debe ser > 0.", frase_pred)
}

handle_hhm_error_df2 <- function() {
  paste0("df₂ debe ser ≥ 1.", frase_pred)
}

handle_hhm_error_alpha <- function() {
  paste0("α debe ser en (0,1).", frase_pred)
}

################################## 
## Excepciones Diana
handle_hhm_error_numero_de_tratamientos_positivo_mayor_que_cero <- function() {
  return("El número de tratamientos debe ser un número entero positivo mayor que cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
}

handle_hhm_error_numero_de_replicas_positivo_mayor_que_cero <- function() {
  return("El número de réplicas iniciales debe ser un número entero positivo mayor que cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")# Para verificar que el mensaje se está generando
}

handle_hhm_error_desviaciones_separas_por_coma <- function() {
  return("Debes ingresar las desviaciones estándar como una lista de números separados por comas. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")}

handle_hhm_error_desviaciones_mal_formateadas <- function() {
  return("Las desviaciones estándar contienen valores no numéricos, negativos o con un formato incorrecto. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
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
  
  # Convertir cadena a vector numérico, eliminando espacios
  sigmas_raw <- strsplit(sigmas_str, ",")[[1]]
  sigmas <- suppressWarnings(as.numeric(trimws(sigmas_raw)))
  
  # Validar que todos los valores sean numéricos y positivos
  if (any(is.na(sigmas)) || any(sigmas <= 0)) {
    show_error(handle_hhm_error_desviaciones_mal_formateadas())
    return(FALSE)
  }
  
  # Validar que la cantidad de sigmas coincida con 'a'
  if (length(sigmas) != a) {
    show_error(handle_hhm_error_cantidad_de_sigmas_distinta_a_tratamientos(a))
    return(FALSE)
  }
  
  return(sigmas)
}

################################## 
handle_hhm_error_presupuesto_total_positivo <- function() {
  return("El presupuesto total debe ser un número positivo mayor que cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
}

handle_hhm_error_cantidad_de_sigmas_distinta_a_tratamientos <- function(a) {
  return(paste0("Debe ingresar exactamente ", a, 
                " desviaciones estándar (σ), separadas por comas, una por cada tratamiento.Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."))
}

handle_hhm_error_costos_separados_por_coma <- function() {
  return("Debe ingresar los costos como una lista de números separados por comas. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
}

handle_hhm_error_costos_mal_formateados <- function() {
  return("Los costos por tratamiento contienen valores no numéricos, negativos o mal formateados. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
}

handle_hhm_error_cantidad_de_costos_distinta_a_tratamientos <- function(a) {
  return(paste0("Debe ingresar exactamente ", a, 
                " costos, uno para cada tratamiento, separados por comas. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."))
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
  
  # Validar 'sigmas_str'
  if (missing(sigmas_str) || !is.character(sigmas_str) || nchar(trimws(sigmas_str)) == 0) {
    show_error(handle_hhm_error_desviaciones_separas_por_coma())
    return(FALSE)
  }
  
  # Procesar sigmas
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
  
  # Validar 'costos_str'
  if (missing(costos_str) || !is.character(costos_str) || nchar(trimws(costos_str)) == 0) {
    show_error(handle_hhm_error_costos_separados_por_coma())
    return(FALSE)
  }
  
  # Procesar costos
  costos_raw <- strsplit(costos_str, ",")[[1]]
  
  # Eliminar posibles espacios extra y convertir a numérico
  costos <- suppressWarnings(as.numeric(trimws(costos_raw)))
  
  # Verificar que todos los costos sean válidos
  if (any(is.na(costos))) {
    show_error("Algunos costos no son válidos. Por favor, ingresa valores numéricos correctamente. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
    return(FALSE)
  }
  
  if (any(costos <= 0)) {
    show_error("Los costos no pueden ser negativos ni cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
    return(FALSE)
  }
  
  if (length(costos) != a) {
    show_error(handle_hhm_error_cantidad_de_costos_distinta_a_tratamientos(a))
    return(FALSE)
  }
  
  return(list(sigmas = sigmas, costos = costos))
}


####################
handle_hhm_error_costo_tratamiento_invalido <- function() {
  return("El costo por tratamiento debe ser un número positivo mayor que cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
}

handle_hhm_error_costo_ue_invalido <- function() {
  return("El costo por unidad experimental debe ser un número positivo mayor que cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
}

handle_hhm_error_sigma_cuadrado_invalido <- function() {
  return("La varianza dentro de los tratamientos debe ser un número positivo mayor que cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
}

handle_hhm_error_rho_invalido <- function() {
  return("La proporción de la varianza total debe estar entre 0 y 1, sin incluirlos. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
}

handle_hhm_error_vmax_invalido <- function() {
  return("La varianza máxima permitida debe ser un número positivo mayor que cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
}

validar_parametros_funcion_disenio <- function(costo_tratamiento, costo_ue, sigma_cuadrado, rho, v_max, show_error) {
  
  if (missing(costo_tratamiento) || !is.numeric(costo_tratamiento) || is.na(costo_tratamiento) || costo_tratamiento <= 0) {
    show_error(handle_hhm_error_costo_tratamiento_invalido())
    return(FALSE)
  }
  
  if (missing(costo_ue) || !is.numeric(costo_ue) || is.na(costo_ue) || costo_ue <= 0) {
    show_error(handle_hhm_error_costo_ue_invalido())
    return(FALSE)
  }
  
  if (missing(sigma_cuadrado) || !is.numeric(sigma_cuadrado) || is.na(sigma_cuadrado) || sigma_cuadrado <= 0) {
    show_error(handle_hhm_error_sigma_cuadrado_invalido())
    return(FALSE)
  }
  
  if (missing(rho) || !is.numeric(rho) || is.na(rho) || rho <= 0 || rho >= 1) {
    show_error(handle_hhm_error_rho_invalido())
    return(FALSE)
  }
  
  if (missing(v_max) || !is.numeric(v_max) || is.na(v_max) || v_max <= 0) {
    show_error(handle_hhm_error_vmax_invalido())
    return(FALSE)
  }
  
  return(TRUE)
}

# Sebastian ---------------------------------------------------------------


Formato_diferencia <- function() {
  paste0("El formato de la diferencia mínima es incorrecto.", frase_pred)
}

Formato_tatamiento <- function() {
  paste0("El formato del tratamiento es incorrecto.", frase_pred)
}

Formato_rho <- function() {
  paste0("El formato del rho es incorrecto.", frase_pred)
}

Formato_sigma <- function() {
  paste0("El formato de la descviación estandar es incorrecta.", frase_pred)
}

Formato_gradosLibertad <- function() {
  paste0("El formato de los grados de libertad es incorrecta.", frase_pred)
}

Formato_Potencia <- function() {
  paste0("El formato de la potencia objetivo es incorrecto.", frase_pred)
}

Formato_significancia <- function() {
  paste0("El formato de la significancia es incorrecto.", frase_pred)
}

Formato_r_max <- function() {
  paste0("El formato del tamaño máximo de muestra es incorrecto.", frase_pred)
}


# Estimacion S1 y df1  ----------------------------------------------------


Formato_S1 <- function() {
  paste0("El formato de la desviación estándar estimada es incorrecto. ", frase_pred)
}

Formato_Si <- function() {
  paste0("El formato del porcentaje inferior relativo (Si) es incorrecto. ", frase_pred)
}

Formato_Ss <- function() {
  paste0("El formato del porcentaje superior relativo (Ss) es incorrecto. ", frase_pred)
}

Formato_intervalo_relativo <- function() {
  paste0("El porcentaje superior relativo (Ss) debe ser mayor que el inferior (Si). ", frase_pred)
}
