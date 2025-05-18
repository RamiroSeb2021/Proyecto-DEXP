
# METODO DE TUKEY ---------------------------------------------------------

head_tukey <- "Cálculo del tamaño muestral por tratamiento según el método de Tukey.

Esta función permite estimar el número de réplicas necesarias por tratamiento en un diseño experimental, 
utilizando el método de Tukey para comparaciones múltiples. El objetivo es garantizar que, al aplicar pruebas 
post-hoc entre tratamientos, sea posible detectar una diferencia mínima significativa previamente establecida 
en la variable de interés. Si dicha diferencia no es detectada en el análisis, se asume que no se alcanzó 
el tamaño muestral requerido para evidenciarla estadísticamente."

# Numero de replicas en el modelo de efectos aleatorios -------------------

# head_Efectos_Aleatorios <- 
  

# Cálculo del tamaño muestral a partir de la potencia ---------------------

head_potencia <- "Calcula el número mínimo de réplicas necesarias para alcanzar una potencia estadística deseada en un diseño ANOVA. Utiliza el número de tratamientos (t), la varianza estimada (σ²), la diferencia mínima detectable (Δ), el nivel de significancia (α) y la potencia objetivo (1−β)."


# HHM ---------------------------------------------------------------------

head_HHM <- "Estima el número de réplicas requeridas a partir de la varianza estimada (S²₁), los grados de libertad (df₂), una diferencia mínima detectable (d) y el nivel de significancia (α), utilizando valores tabulados del método HHM."
