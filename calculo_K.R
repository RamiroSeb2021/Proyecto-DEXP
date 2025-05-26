# Si hace falta, instalar:
# if (!require("stats")) install.packages("stats")

calcular_k_tabla <- function(n, m,
                             alpha = 0.05,
                             beta  = 0.95,
                             precision = 1e-6,
                             k_max     = 200,
                             n_bracket = 100) {
  # Estadístico crítico de t
  t_crit <- qt(1 - alpha, df = n)
  
  # Función que para un k dado devuelve (potencia - beta)
  objetivo <- function(k) {
    # Integrando sobre F ~ F(n, m)
    integrando <- function(f) {
      # densidad de F(n, m)
      d_f <- df(f, df1 = n, df2 = m)
      # shift de la media en la t bajo alternativa
      media_nueva <- k * sqrt(n + 1) / sqrt(f)
      # potencia condicional (unilateral)
      potencia_f <- 1 - pt(t_crit - media_nueva, df = n)
      potencia_f * d_f
    }
    # integración numérica de 0 a Inf
    pot_total <- integrate(integrando, lower = 0, upper = Inf,
                           rel.tol = precision)$value
    pot_total - beta
  }
  
  # 1) muestrear k para encontrar un bracket donde objetivo cambie de signo
  ks <- seq(0.001, k_max, length.out = n_bracket)
  fs <- sapply(ks, objetivo)
  idx <- which(diff(sign(fs)) != 0)
  if (length(idx) == 0) {
    stop("No se encontró bracket en [0.001, ", k_max, "], prueba aumentando k_max.")
  }
  k_lo <- ks[idx[1]]
  k_hi <- ks[idx[1] + 1]
  
  # 2) usar uniroot en ese bracket
  sol <- uniroot(objetivo, interval = c(k_lo, k_hi), tol = precision)
  return(sol$root)
}

# Ejemplo de uso:
# Para n = 1, m = 1, alfa = 0.05, beta = 0.95 (Tabla II, fila n=1 col m=1 ≈ 57.1)
k <- calcular_k_tabla(n = 1, m = 1, alpha = 0.05, beta = 0.8)
cat("k =", round(k, 2), "\n")

for (i in c(1:6, 8, 12, 16, 24)){
  print(calcular_k_tabla(n = 1, m = i, alpha = 0.05, beta = 0.8))
}


# Version 2 ---------------------------------------------------------------

# Función refinada para calcular k = a/s1
calcular_k_tabla <- function(n, m,
                             alpha     = 0.05,
                             beta      = 0.80,
                             precision = 1e-8,
                             k_max     = 200,
                             n_bracket = 1000) {
  # 1) Cálculo del crítico t
  t_crit <- qt(1 - alpha, df = n)
  
  # 2) Cota superior para la integración de F
  #    usamos un quantil muy extremo para cubrir casi toda la masa
  upperF <- qf(1 - 1e-8, df1 = n, df2 = m)
  
  # 3) Función objetivo: potencia(k) - beta
  objetivo <- function(k) {
    integrando <- function(f) {
      # densidad de F(n, m)
      dF <- df(f, df1 = n, df2 = m)
      # media desplazada de t bajo la alternativa
      shift <- k * sqrt(n + 1) / sqrt(f)
      # potencia condicional dado f
      pot_f <- 1 - pt(t_crit - shift, df = n)
      pot_f * dF
    }
    # integración numérica de 0 a upperF
    potencia_total <- integrate(integrando,
                                lower       = 0,
                                upper       = upperF,
                                rel.tol     = precision,
                                subdivisions = 2000)$value
    potencia_total - beta
  }
  
  # 4) Muestreo de k para encontrar un intervalo donde objetivo() cambie de signo
  ks <- seq(1e-3, k_max, length.out = n_bracket)
  fs <- sapply(ks, objetivo)
  idx <- which(diff(sign(fs)) != 0)
  if (length(idx) == 0) {
    stop("No se encontró cambio de signo en [1e-3, ", k_max, "]. Aumenta k_max o n_bracket.")
  }
  k_lo <- ks[idx[1]]
  k_hi <- ks[idx[1] + 1]
  
  # 5) Refinar con uniroot en ese subintervalo
  sol <- uniroot(objetivo, interval = c(k_lo, k_hi), tol = precision)
  sol$root
}

# ———————— Pruebas —————————

# Caso n = 1, m = 1, alfa = 0.05, beta = 0.80 (Tabla I, valor ≈ 13.8)
k1 <- calcular_k_tabla(n = 1, m = 1, alpha = 0.05, beta = 0.80)
cat("n=1, m=1, α=0.05, β=0.80 → k =", round(k1, 2), "\n")

# Caso n = 1, m = 1, alfa = 0.05, beta = 0.95 (Tabla II, valor ≈ 57.1)
k2 <- calcular_k_tabla(n = 1, m = 1, alpha = 0.05, beta = 0.95)
cat("n=1, m=1, α=0.05, β=0.95 → k =", round(k2, 2), "\n")



# Intetnto 3 --------------------------------------------------------------

calcular_k_tabla <- function(n, m,
                             alpha     = 0.05,
                             beta      = 0.80,
                             precision = 1e-6,
                             k_max     = 200,
                             n_bracket = 500) {
  # 1) crítico t
  t_crit <- qt(1 - alpha, df = n)
  
  # 2) límites para F: arranco en f_min > 0 y corto cola arriba
  f_min  <- 1e-8
  f_max  <- qf(1 - 1e-6, df1 = n, df2 = m)
  
  # 3) función objetivo(k) = potencia(k) - beta
  objetivo <- function(k) {
    integrando <- function(f) {
      dF    <- df(f, df1 = n, df2 = m)
      shift <- k * sqrt(n + 1) / sqrt(f)
      # potencia condicional
      pot_f <- 1 - pt(t_crit - shift, df = n)
      pot_f * dF
    }
    # integrar evitando f=0 exactamente
    pot_total <- integrate(integrando,
                           lower       = f_min,
                           upper       = f_max,
                           rel.tol     = precision,
                           subdivisions = 500)$value
    pot_total - beta
  }
  
  # 4) buscar un bracket en k donde objetivo() cambie de signo
  ks <- seq(1e-3, k_max, length.out = n_bracket)
  fs <- vapply(ks, objetivo, numeric(1))
  idx <- which(diff(sign(fs)) != 0)
  if (!length(idx)) {
    stop("No se encontró cambio de signo en k ∈ [0.001, ", k_max, "]. Aumenta k_max o n_bracket.")
  }
  k_lo <- ks[idx[1]]
  k_hi <- ks[idx[1] + 1]
  
  # 5) refinar con uniroot
  sol <- uniroot(objetivo, interval = c(k_lo, k_hi), tol = precision)
  sol$root
}

# ————————— Prueba rápida —————————

k_test <- calcular_k_tabla(n = 1, m = 1:6, alpha = 0.05, beta = 0.80)
cat("Resultado esperado ≈13.8 → k =", round(k_test, 2), "\n")



# intento n! --------------------------------------------------------------

calcular_k_tabla_mc <- function(n, m,
                                alpha = 0.05,
                                beta  = 0.80,
                                B     = 200000,   # réplicas MC
                                seed  = 1234,
                                tol   = 0.01) {   # precisión en beta
  set.seed(seed)
  tcrit <- qt(1 - alpha, df = n)
  
  # simulación previa de s1 y s2
  s1s <- sqrt(rchisq(B, df = m) / m)
  s2s <- sqrt(rchisq(B, df = n) / n)
  
  # función que, dado k, devuelve pot - beta
  objetivo <- function(k) {
    # diferencia real por réplica
    a   <- k * s1s
    # medias muestrales
    xbs <- rnorm(B, mean = a, sd = 1 / sqrt(n + 1))
    ts  <- sqrt(n + 1) * xbs / s2s
    mean(ts > tcrit) - beta
  }
  
  # bracket en k para uniroot
  # buscamos donde objetivo() cruza cero
  ks <- seq(0.1, 200, length.out = 200)
  fs <- sapply(ks, objetivo)
  idx <- which(diff(sign(fs)) != 0)[1]
  if (is.na(idx)) stop("No converge: ajusta rango de k o B.")
  
  k_lo <- ks[idx]
  k_hi <- ks[idx + 1]
  
  sol <- uniroot(objetivo, interval = c(k_lo, k_hi), tol = tol)
  sol$root
}

# Ejemplo para n = 1, m = 1, alfa = 0.05, beta = 0.80
k_est <- calcular_k_tabla_mc(n = 1, m = 2, alpha = 0.05, beta = 0.80,
                             B = 200000, seed = 2025, tol = 0.005)
cat("Estimación Monte Carlo de k ≈", round(k_est, 2), "\n")
# Debería darte algo cercano a 13.8

for (i in 1:10){
  print(i)
  print(calcular_k_tabla_mc(n = i, m = 1, alpha = 0.05, beta = 0.8))
}


# intento (n + 1)! --------------------------------------------------------

calcular_k_tabla_mc <- function(n, m,
                                alpha = 0.05,
                                beta  = 0.80,
                                B     = 1e6,      # ahora un millón de réplicas
                                seed  = 1234,
                                tol   = 0.001) {   # tolerancia en beta
  
  set.seed(seed)
  tcrit <- qt(1 - alpha, df = n)
  
  # 1) Simulamos s1 y s2 de sus χ²
  s1s <- sqrt(rchisq(B, df = m) / m)
  s2s <- sqrt(rchisq(B, df = n) / n)
  
  # 2) Función objetivo(k) = pot(k) - beta
  objetivo <- function(k) {
    a    <- k * s1s
    xbs  <- rnorm(B, mean = a, sd = 1 / sqrt(n + 1))
    ts   <- sqrt(n + 1) * xbs / s2s
    mean(ts > tcrit) - beta
  }
  
  # 3) Encontrar bracket en k
  ks <- seq(5, 25, length.out = 500)   # sabemos que para n=m=1 está entre 5 y 25
  fs <- vapply(ks, objetivo, numeric(1))
  idx <- which(diff(sign(fs)) != 0)[1]
  if (is.na(idx)) stop("No converge en [5,25], ajusta rango o B.")
  
  # 4) Refinar raíz con uniroot
  sol <- uniroot(objetivo,
                 interval = c(ks[idx], ks[idx+1]),
                 tol      = tol)
  sol$root
}

# Prueba para n=1, m=1, alfa=0.05, beta=0.80
k_est <- calcular_k_tabla_mc(n = 10, m = 1,
                             alpha = 0.05, beta = 0.80,
                             B     = 1e9, seed = 2025, tol = 0.001)
cat("Con B=1e6 → k ≈", round(k_est, 2), "\n")
# Ahora deberías ver algo muy cercano a 13.8

for (i in c(1:6, 8, 12, 16, 24, 32)){
  print(calcular_k_tabla_mc(n = 1, m = i,
                            alpha = 0.05, beta = 0.80,
                            B     = 200000, seed = 2025, tol = 0.001))
}


# Investigacion profunda --------------------------------------------------

calc_k_integration <- function(n, m, beta = 0.80, alpha = 0.05) {
  # Función integrando la densidad conjunta t-F para hallar k = a/s1 con potencia = beta
  # n: grados de libertad del efecto (numerador), m: g.l. del error (denominador)
  
  # Umbral crítico de F para la significancia alpha (prueba unilateral)
  Fcrit <- qf(1 - alpha, df1 = n, df2 = m)
  
  # Potencia (función objetivo) dado un parámetro no central phi
  power_for_phi <- function(phi) {
    integrand <- function(y) {
      # f_Y densidad chi-cuadrado (error), 1 - F_X es cola superior de chi-cuadrado no central (efecto)
      (1 - pchisq(q = Fcrit * (n/m) * y, df = n, ncp = phi)) * dchisq(y, df = m)
    }
    # Integrar de 0 a Inf
    val <- integrate(integrand, lower = 0, upper = Inf, subdivisions = 2000, rel.tol = 1e-8)$value
    return(val)
  }
  
  # Buscar phi tal que potencia = beta
  # Establecer cotas de búsqueda para phi (ncp). Comenzar en 0 (sin efecto) y un máximo alto
  lower_phi <- 0
  upper_phi <- 1000
  # Asegurar que a phi=0 la potencia < beta, y a phi=upper la potencia > beta
  low_pow <- power_for_phi(lower_phi)
  high_pow <- power_for_phi(upper_phi)
  if (low_pow > beta) {
    stop("Potencia con phi=0 ya excede beta; revisar parámetros.")
  }
  # Extender cota superior si aún no alcanza la potencia deseada
  while (high_pow < beta) {
    lower_phi <- upper_phi
    upper_phi <- upper_phi * 2
    high_pow <- power_for_phi(upper_phi)
    if (upper_phi > 1e6) stop("Se requiere phi muy grande, posible problema de convergencia.")
  }
  
  # Función para uniroot: diferencia entre potencia(phi) y beta
  f_root <- function(phi) power_for_phi(phi) - beta
  
  # Encontrar raíz
  phi_star <- uniroot(f_root, lower = lower_phi, upper = upper_phi, tol = 1e-5)$root
  
  # Convertir phi a k = a/s1.
  # Caso df1 = 1 (comparación de dos medias): utilizar tamaños de muestra (aprox. balanceados) para calcular k.
  if (n == 1) {
    # Suponer dos grupos: si m es par, ambos grupos de tamaño (m/2 + 1); si m impar, tamaños difieren en 1
    if (m %% 2 == 0) {
      n1 <- m/2 + 1
      n2 <- n1
    } else {
      n1 <- floor(m/2) + 2   # grupo con +1 obs.
      n2 <- floor(m/2) + 1
    }
    # Delta (no centralidad t) es sqrt(phi), y k = delta * sqrt(1/n1 + 1/n2)
    delta <- sqrt(phi_star)
    k <- delta * sqrt(1/n1 + 1/n2)
  } else {
    # Caso general ANOVA: suponer replicación equilibrada (o semi-equilibrada)
    # Replicas efectivas r = m/(n+?)+1; aquí usamos r = m/(n+1)+1 (promedio de replicaciones por tratamiento)
    r <- m/(n + 1) + 1
    # Relación teórica: phi = (r * (a^2))/(2*sigma^2)  =>  a/sigma = sqrt(2*phi / r)
    k <- sqrt(2 * phi_star / r)
  }
  return(k)
}


# Ejemplos de uso del método de integración:
cases <- list(c(1,1), c(1,2), c(4,4), c(8,8), c(16,16))
for (pair in cases) {
  n <- pair[1]; m <- pair[2]
  k_val <- calc_k_integration(n, m, beta = 0.80, alpha = 0.05)
  cat(sprintf("(%d, %d): k = %.3f\n", n, m, k_val))
}

# Funcion corregida.

# -----------------------------------------------------------
# Cálculo EXACTO del k = a/s1 de la Tabla I (β = 0.80, α = 0.05)
# -----------------------------------------------------------
k_tabla_exact <- function(n, m,
                          beta   = 0.80,
                          alpha  = 0.05,
                          rel.tol = 1e-8) {
  tcrit <- qt(1 - alpha, df = n)         # crítico t unilateral
  
  # --- potencia como función de D ---
  potencia_D <- function(D) {
    integrand <- function(f) {
      dF    <- df(f, df1 = n, df2 = m)   # densidad F(n,m)
      delta <- D / sqrt(f)               # ncp de t condicional
      (1 - pt(tcrit - delta, df = n)) * dF
    }
    eps   <- 1e-10                       # evitar f = 0 exacto
    f_max <- qf(1 - 1e-10, n, m)         # cortar cola superior (1-1e-10 de masa)
    integrate(integrand,
              lower = eps,
              upper = f_max,
              subdivisions = 500,
              rel.tol = rel.tol)$value
  }
  
  # --- búsqueda de D tal que potencia = beta ---
  f_obj <- function(D) potencia_D(D) - beta
  
  D_hi <- 1
  while (f_obj(D_hi) < 0) D_hi <- D_hi * 2   # ampliar hasta potencia > β
  D_star <- uniroot(f_obj, c(0, D_hi), tol = 1e-7)$root
  
  # k = D / √(n+1)
  k <- D_star / sqrt(n + 1)
  return(k)
}

# --------- Verificación de la primera fila completa (n = 1) ----------
fila_n1 <- sapply(c(1:6, 8, 12, 16, 24, 32),
                  function(m) k_tabla_exact(1, m, rel.tol = 1e-4))
names(fila_n1) <- c(1:6, 8, 12, 16, 24, 32)
print(round(fila_n1, 2))

k_tabla_exact(1, 1, rel.tol = 1e-2 )


# La que es  --------------------------------------------------------------


k_tabla_mc <- function(n, m, beta = 0.80, alpha = 0.05,
                       B = 2e6, seed = 1) {
  set.seed(seed)
  tcrit <- qt(1 - alpha, df = n)
  s1 <- sqrt(rchisq(B, m) / m)
  s2 <- sqrt(rchisq(B, n) / n)
  
  potencia <- function(k) {
    a <- k * s1
    xbar <- rnorm(B, mean = a, sd = 1 / sqrt(n + 1))
    mean((sqrt(n + 1) * xbar / s2) > tcrit)
  }
  uniroot(function(k) potencia(k) - beta,
          c(0, 100), tol = 0.001)$root
}

# Ejemplo rápido
k_mc <- k_tabla_mc(1, 1, B = 5e5, beta = 0.95)  # media ±0.1 de 13.8 con 5e5 réplicas
print(k_mc)

fila_n1 <- sapply(c(1:10),
                  function(m) k_tabla_mc(m, 1, B = 5e5))
names(fila_n1) <- c(1:10)
print(round(fila_n1, 2))


# Matriz 

# Definir vectores de n y m
n_vals <- 1:10
m_vals <- c(1:6, 8, 12, 16, 24, 32)

# Inicializar matriz vacía
k_matrix <- matrix(NA, nrow = length(n_vals), ncol = length(m_vals),
                   dimnames = list(paste0("n=", n_vals), paste0("m=", m_vals)))

# Calcular cada valor de k usando k_tabla_mc()
for (i in seq_along(n_vals)) {
  for (j in seq_along(m_vals)) {
    n <- n_vals[i]
    m <- m_vals[j]
    k_matrix[i, j] <- k_tabla_mc(n, m, B = 5e5, beta = 0.95)  # ajustar B y tol según necesidad
  }
}

# Convertir a data frame (opcional, para visualización o exportación)
k_df <- as.data.frame(k_matrix)
k_df <- cbind(n = n_vals, k_df)  # agregar columna n como identificador

# Mostrar
print(k_df, digits = 4)

View(round(k_df[-1], 3))


# Ajuste ------------------------------------------------------------------

k_tabla_exact <- function(n, m,
                          beta    = 0.80,
                          alpha   = 0.05,
                          rel.tol = 1e-4) {
  tcrit <- qt(1 - alpha, df = n)
  
  potencia_D <- function(D) {
    integrand <- function(f) {
      dF    <- df(f, df1 = n, df2 = m)
      delta <- D / sqrt(f)
      (1 - pt(tcrit - delta, df = n)) * dF
    }
    eps   <- 1e-6
    f_max <- qf(1 - 1e-6, n, m)  # 99.9999% de la masa
    integrate(integrand,
              lower = eps,
              upper = f_max,
              subdivisions = 300,
              rel.tol = rel.tol)$value
  }
  
  f_obj <- function(D) potencia_D(D) - beta
  
  D_hi <- 5
  while (f_obj(D_hi) < 0) D_hi <- D_hi * 1.5
  D_star <- uniroot(f_obj, c(0.01, D_hi), tol = 1e-4)$root
  
  k <- D_star / sqrt(n + 1)
  return(k)
}

