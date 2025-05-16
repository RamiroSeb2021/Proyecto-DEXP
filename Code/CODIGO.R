############### METODO DE POTENCIA #################
# Función para calcular el número de réplicas usando el método de potencia
calcular_replicas_potencia <- function(alpha, potencia, delta, sigma2, t) {
  # Parámetros iniciales
  beta <- 1 - potencia
  nu1 <- t - 1
  
  # Valor inicial de r
  r <- 2
  encontrado <- FALSE
  max_iter <- 100
  iter <- 0
  
  while(!encontrado && iter < max_iter) {
    iter <- iter + 1
    
    # Calcular phi^2
    phi2 <- (r * delta^2) / (2 * t * sigma2)
    phi <- sqrt(phi2)
    
    # Grados de libertad para el error
    nu2 <- t * (r - 1)
    
    # Calcular beta aproximado usando la distribución F no central
    # Usamos la función pf() de R para la distribución F no central
    f_critico <- qf(1 - alpha, nu1, nu2)
    beta_calculado <- pf(f_critico, nu1, nu2, ncp = phi2)
    
    # Comparar con el beta deseado
    if(beta_calculado <= beta) {
      encontrado <- TRUE
    } else {
      r <- r + 1
    }
  }
  
  return(list(r = r, phi = phi, beta_calculado = beta_calculado))
}

# Ejemplo de uso:
alpha <- 0.05
potencia <- 0.80
delta <- 3
sigma2 <- 10.35
t <- 4

resultado <- calcular_replicas_potencia(alpha, potencia, delta, sigma2, t)
print(paste("Número de réplicas necesarias:", resultado$r))

################
# Parámetros de entrada (ejemplo ficticio)
alpha <- 0.05            # Nivel de significancia
power_target <- 0.80     # Potencia deseada (1 - beta)
t <- 4                   # Número de tratamientos => df1 = t - 1 = 3
sigma2 <- 10.35          # Varianza estimada del error
# Supongamos que, a partir de la información experimental,
# se tiene que la suma de cuadrados de los efectos es:
SCE <- 33.156         

# Función que calcula la potencia para un valor r dado
potencia_ANOVA <- function(r, alpha, t, sigma2, SCE) {
  df1 <- t - 1
  df2 <- t * (r - 1)
  # Suponga que la relación efecto/varianza se expresa:
  # ncp (parámetro de no centralidad) = r * (SCE) / (constante * sigma2)
  # En el ejemplo del libro se llegó a φ² = (5*SCE)/(4*sigma2),
  # lo cual se obtiene al tomar r = 5. En el enfoque general,
  # definimos una constante "K" en función del experimento.
  #
  # Para efectos del ejemplo, si se usa la aproximación de Scheffé:
  # Usando Δ (diferencia mínima) se tiene:
  # ncp = r * Δ² / (2*t*sigma2)
  # Aquí haremos dos variantes:
  
  # Variante A: Usando directamente SCE como indicador del efecto
  ncp_A <- r * SCE / sigma2
  
  # Variante B: Usando el enfoque de Scheffé con Δ (se debe especificar Δ)
  # Por ejemplo, si Δ = 3 (como en el ejemplo 5.9)
  Delta <- 3
  ncp_B <- r * Delta^2 / (2 * t * sigma2)
  
  # Se calcula el valor crítico de la F para df1 y df2:
  F_crit <- qf(1 - alpha, df1, df2)
  # Se puede calcular la potencia:
  potencia_A <- 1 - pf(F_crit, df1, df2, ncp = ncp_A)
  potencia_B <- 1 - pf(F_crit, df1, df2, ncp = ncp_B)
  
  return(list(r = r, potencia_A = potencia_A, potencia_B = potencia_B))
}

# Se puede usar una búsqueda iterativa para hallar el valor mínimo de r que alcance la potencia deseada:
buscar_r <- function(alpha, t, sigma2, SCE, power_target, Delta = 3) {
  r <- 2  # comenzamos con un valor mínimo
  repeat {
    df1 <- t - 1
    df2 <- t * (r - 1)
    # Usando la aproximación de Scheffé para ncp:
    ncp <- r * Delta^2 / (2 * t * sigma2)
    F_crit <- qf(1 - alpha, df1, df2)
    potencia <- 1 - pf(F_crit, df1, df2, ncp = ncp)
    if (potencia >= power_target) break
    r <- r + 1
  }
  return(r)
}

# Ejemplo de uso:
r_necesario <- buscar_r(alpha = 0.05, t = 4, sigma2 = 10.35, SCE = 33.156, 
                        power_target = 0.80, Delta = 3)
cat("El número mínimo de réplicas por tratamiento es:", r_necesario, "\n")


################ Metodo Harris-Hurvitz-Mood ##########
# Función para calcular el número de réplicas usando el método HHM
calcular_replicas_HHM <- function(S1, df1, d, potencia, alpha, t) {
  # Buscar el valor K en la tabla (aquí aproximado)
  # En la práctica deberías tener una tabla precisa o una función de interpolación
  if(potencia == 0.80) {
    K <- 0.322  # Valor aproximado para potencia 80%
  } else if(potencia == 0.95) {
    K <- 0.502  # Valor aproximado para potencia 95%
  } else {
    stop("Potencia no soportada. Use 0.80 o 0.95")
  }
  
  # Estimar df2 inicialmente como df1
  df2 <- df1
  
  # Calcular r
  r <- 2 * (df2 + 1) * (K * S1 / d)^2
  
  # Redondear hacia arriba
  r <- ceiling(r)
  
  return(r)
}

# Ejemplo de uso:
S1 <- sqrt(141.6)  # Desviación estándar
df1 <- 40
d <- 20
potencia <- 0.80
alpha <- 0.05
t <- 6

r_HHM <- calcular_replicas_HHM(S1, df1, d, potencia, alpha, t)
print(paste("Número de réplicas necesarias (HHM):", r_HHM))

##########
# Parámetros del método HHM
S1 <- 2.85  # Valor estimado (por ejemplo, promedio de SI y SS)
d <- 20     # Diferencia mínima significativa
K <- 0.322  # Valor de tabla (debe consultarse según el diseño)
df2 <- 60   # Grados de libertad para S2

# Cálculo del número de réplicas
r_HHM <- 2 * (df2 + 1) * (K * S1 / d)^2
r_HHM_rounded <- ceiling(r_HHM)

cat("El número de réplicas requerido (método HHM) es:", r_HHM_rounded)

######
cambio 
