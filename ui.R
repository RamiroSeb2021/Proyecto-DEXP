library(shiny)
library(shinythemes)
source("Code/metodo_tukey.R")
source("Code/Costos.R")
source("Code/Potencia_Efectos_Aleatorios.R")
source("Code/Potencia.R") 
source("Code/metodo_Harris_Hurvitz_Mood.R") 
tabsetPanel(
  tabPanel("Proporcionalidad sin costo", 
           h4("Parámetros"),
           numericInput("a", "Tratamientos (a)", 4),
           numericInput("r0", "Réplicas iniciales (r₀)", 5),
           textInput("sigmas", "Desviaciones σ (separadas por comas)", "6.27,9.57,12,3.32"),
           actionButton("calcular_1", "Calcular", class = "btn btn-success")
  ),
  
  tabPanel("Proporcionalidad con costo", 
           h4("Parámetros"),
           numericInput("a_2", "Tratamientos (a)", 4),
           textInput("sigmas_2", "Desviaciones σ (vector)", "6.27,9.57,12,3.32"),
           textInput("costos", "Costos por tratamiento (vector)", "1000,200,700,1100"),
           numericInput("costo_total", "Presupuesto total", 50000),
           actionButton("calcular_2", "Calcular", class = "btn btn-success")
  ),
  
  tabPanel("Efectos aleatorios", 
           h4("Parámetros"),
           numericInput("costo_tratamiento", "Costo tratamiento (C₁)", 150000),
           numericInput("costo_ue", "Costo unidad (C₂)", 50000),
           numericInput("sigma_cuadrado", "Varianza error (σ²)", 416.21),
           numericInput("rho", "Proporción ρ", 0.3796),
           numericInput("v_max", "Varianza máxima (v_max)", 43.49),
           actionButton("calcular_3", "Calcular", class = "btn btn-success")
  ),
  
  # Nuevo: Cálculo de Potencia
  tabPanel("Cálculo de Potencia", 
           h4("Parámetros"),
           numericInput("r_potencia", "Réplicas por tratamiento (r)", 15),
           numericInput("t_potencia", "Tratamientos (t)", 4),
           numericInput("sigma2_potencia", "Varianza estimada (σ²)", 10.35),
           numericInput("Delta_potencia", "Diferencia mínima detectable (Δ)", 3),
           numericInput("alpha_potencia", "Nivel de significancia (α)", 0.05),
           actionButton("calcular_4", "Calcular", class = "btn btn-success")
  ),
  
  # Nuevo: Método Harris–Hurvitz–Mood (HHM)
  tabPanel("Método HHM", 
           h4("Parámetros"),
           numericInput("S1_hhm", "Desviación estándar experimental (S₁)", sqrt(141.6)),
           numericInput("d_hhm", "Diferencia mínima detectable (d)", 20),
           numericInput("df2_hhm", "Grados de libertad (df₂)", 60),
           numericInput("K_hhm", "Valor K (tabla A.9)", 0.322),
           actionButton("calcular_5", "Calcular", class = "btn btn-success")
  )
)
