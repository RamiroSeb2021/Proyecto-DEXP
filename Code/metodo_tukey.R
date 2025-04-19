
# Contar decimales --------------------------------------------------------


contar_decimales <- function(x) {
  # Manejo de números enteros
  if (x %% 1 == 0) {
    return(0)
  }
  # Convertir a texto sin notación científica
  x_str <- format(x, scientific = FALSE)
  # Separar por el punto decimal
  partes <- strsplit(x_str, "\\.")[[1]]
  # Contar cuántos dígitos hay después del punto
  nchar(partes[2])
}


# Calcular desviacion estandar y grados de libertad  ----------------------


calcular_S1_df1 <- function(vector_interes, 
                            Si, 
                            Ss, 
                            max_error = 0.01,
                            confianza = 0.95){
  ## Documentacion
  # Vector_interes: Variable a contrastar mediante los
  #                 tratamientos
  # Si: Desviacion inferior de la desviacion estandar 
  # Ss: Desviacion superior de la desviacion estandar 
  # max_error: Max error permitido para df1 llegado el caso
  #            no se encuentre el valor exacto   
  # 
  
  media <- mean(vector_interes)
  n <- length(vector_interes)
  media_sd <- media/sqrt(n)
  SI <- media_sd * Si
  SS <- media_sd * Ss
  
  S1 <- (SI + SS)/2
  
  Cociente <- round(SS/SI, 2)
  
  x <- numeric(n)
  for(i in 1:n){
    x[i] <- 
      round(sqrt(qchisq(p = 0.9, df = i)/qchisq(p = 0.1, df = i)), 2)
    error <- abs(x[i] - Cociente)/Cociente
    if(x[i] == Cociente |  error < max_error){
      return(list(Media = media,
                  S1 = S1,
                  grados_libertad = i,
                  valor_x = x[i],
                  error_relativo = error))
    }
  }
  return(NULL)
} 


# Prueba1 ------------------------------------------------------------------

set.seed(123)
vector_ <- rnorm(1000, mean = 30, sd = 6)

calcular_S1_df1(vector_interes = vector_, Si = 0.07, Ss = 0.12)


