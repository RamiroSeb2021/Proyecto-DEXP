# prueba de potencia 
source("Code/metodo_tukey.R")
set.seed(123)
vector_ <- rnorm(1000, mean = 948.6833, sd = 30)
calcular_S1_df1(vector_interes = vector_, Si = 0.07, Ss = 0.12)
ja