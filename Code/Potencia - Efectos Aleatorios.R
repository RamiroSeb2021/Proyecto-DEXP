simular_potencia <- function(a, n, sigma2 = 1, rho = 0.5, alpha = 0.05, nsim = 1000) {
  sigma2_tau <- rho * sigma2
  f_crit <- qf(1 - alpha, df1 = a - 1, df2 = a * (n - 1))
  rechazos <- numeric(nsim)
  
  for (i in 1:nsim) {
    tau <- rnorm(a, mean = 0, sd = sqrt(sigma2_tau))
    datos <- vector()
    for (j in 1:a) {
      errores <- rnorm(n, mean = 0, sd = sqrt(sigma2))
      y <- tau[j] + errores
      datos <- c(datos, y)
    }
    grupo <- factor(rep(1:a, each = n))
    modelo <- aov(datos ~ grupo)
    F_obs <- summary(modelo)[[1]]["grupo", "F value"]
    rechazos[i] <- as.numeric(F_obs > f_crit)
  }
  
  mean(rechazos)
}

encontrar_n_minimo <- function(a, rho, potencia_objetivo = 0.8, 
                               sigma2 = 1, alpha = 0.05, nsim = 1000, 
                               n_max = 50) {
  for (n in 2:n_max) {
    cat("Probando n =", n, "...\n")
    potencia <- simular_potencia(a = a, n = n, sigma2 = sigma2, 
                                 rho = rho, alpha = alpha, nsim = nsim)
    cat("Potencia estimada:", round(potencia, 4), "\n")
    if (potencia >= potencia_objetivo) {
      cat("Se alcanza potencia >= ", potencia_objetivo, " con n =", n, "\n")
      return(list(n_optimo = n, potencia = potencia))
    }
  }
  warning("No se alcanzó la potencia deseada con n_max = ", n_max)
  return(NULL)
}
