# library(shiny)
# library(shinydashboard)
# 
# dashboard_ui <-dashboardPage(
#   skin = "green",
#   dashboardHeader(title = "Diseño Experimental"),
#   dashboardSidebar(
#     sidebarMenu(id = "tabs",
#                 menuItem("Introducción", tabName = "intro", icon = icon("info-circle")),
#                 menuItem("Cálculos número de réplicas", icon = icon("flask"),
#                          menuSubItem("Proporcionalidad sin Costo", tabName = "sin_costo"),
#                          menuSubItem("Proporcionalidad con Costo", tabName = "con_costo"),
#                          menuSubItem("Efectos Aleatorios", tabName = "efectos"),
#                          menuSubItem("Cálculo de Potencia", tabName = "potencia"),
#                          menuSubItem("Método HHM", tabName = "hhm")
#                 )
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "intro",
#               fluidRow(
#                 box(title = "Bienvenido", width = 12, status = "primary", solidHeader = TRUE,
#                     p("A continuación podrás realizar cálculos para determinar el número de réplicas en diferentes contextos de diseño experimental. Estos incluyen métodos con y sin costos, con efectos aleatorios, cálculos de potencia, y el método de Harris-Hurvitz-Mood (HHM)."),
#                     p("Haz clic en 'Continuar' para comenzar con el primer cálculo: Proporcionalidad sin Costo."),
#                     actionButton("continuar", "Continuar", class = "btn btn-primary")
#                 )
#               )
#       ),
#       tabItem(tabName = "sin_costo",
#               fluidRow(
#                 box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
#                     numericInput("a", "Tratamientos (a)", 4),
#                     numericInput("r0", "Réplicas iniciales (r₀)", 5),
#                     textInput("sigmas", "Desviaciones σ (separadas por comas)", "6.27,9.57,12,3.32"),
#                     actionButton("calcular_1", "Calcular", class = "btn btn-success")
#                 ),
#                 box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
#                     verbatimTextOutput("resultados_1")
#                 )
#               ),
#               fluidRow(
#                 column(12, align = "right",
#                        actionButton("siguiente_1", "Siguiente", icon = icon("arrow-right"), class = "btn btn-primary")
#                 )
#               )
#       ),
#       tabItem(tabName = "con_costo",
#               fluidRow(
#                 box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
#                     numericInput("a_2", "Tratamientos (a)", 4),
#                     textInput("sigmas_2", "Desviaciones σ", "6.27,9.57,12,3.32"),
#                     textInput("costos", "Costos por tratamiento", "1000,200,700,1100"),
#                     numericInput("costo_total", "Presupuesto total", 50000),
#                     actionButton("calcular_2", "Calcular", class = "btn btn-success")
#                 ),
#                 box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
#                     verbatimTextOutput("resultados_2")
#                 )
#               ),
#               fluidRow(
#                 column(6, align = "left",
#                        actionButton("anterior_2", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
#                 ),
#                 column(6, align = "right",
#                        actionButton("siguiente_2", "Siguiente", icon = icon("arrow-right"), class = "btn btn-primary")
#                 )
#               )
#       ),
#       tabItem(tabName = "efectos",
#               fluidRow(
#                 box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
#                     numericInput("costo_tratamiento", "Costo tratamiento (C₁)", 150000),
#                     numericInput("costo_ue", "Costo unidad experimental (C₂)", 50000),
#                     numericInput("sigma_cuadrado", "Varianza del error (σ²)", 416.21),
#                     numericInput("rho", "Proporción ρ", 0.3796),
#                     numericInput("v_max", "Varianza máxima (v_max)", 43.49),
#                     actionButton("calcular_3", "Calcular", class = "btn btn-success")
#                 ),
#                 box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
#                     verbatimTextOutput("resultados_3")
#                 )
#               ),
#               fluidRow(
#                 column(6, align = "left",
#                        actionButton("anterior_3", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
#                 ),
#                 column(6, align = "right",
#                        actionButton("siguiente_3", "Siguiente", icon = icon("arrow-right"), class = "btn btn-primary")
#                 )
#               )
#       ),
#       tabItem(tabName = "potencia",
#               fluidRow(
#                 box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
#                     numericInput("r_potencia", "Réplicas por tratamiento (r)", 15),
#                     numericInput("t_potencia", "Tratamientos (t)", 4),
#                     numericInput("sigma2_potencia", "Varianza estimada (σ²)", 10.35),
#                     numericInput("Delta_potencia", "Diferencia mínima detectable (Δ)", 3),
#                     numericInput("alpha_potencia", "Nivel de significancia (α)", 0.05),
#                     actionButton("calcular_4", "Calcular", class = "btn btn-success")
#                 ),
#                 box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
#                     verbatimTextOutput("resultados_4")
#                 )
#               ),
#               fluidRow(
#                 column(6, align = "left",
#                        actionButton("anterior_4", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
#                 ),
#                 column(6, align = "right",
#                        actionButton("siguiente_4", "Siguiente", icon = icon("arrow-right"), class = "btn btn-primary")
#                 )
#               )
#       ),
#       tabItem(tabName = "hhm",
#               fluidRow(
#                 box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
#                     numericInput("S1_hhm", "Desviación estándar experimental (S₁)", sqrt(141.6)),
#                     numericInput("d_hhm", "Diferencia mínima detectable (d)", 20),
#                     numericInput("df2_hhm", "Grados de libertad (df₂)", 60),
#                     numericInput("K_hhm", "Valor K (tabla A.9)", 0.322),
#                     actionButton("calcular_5", "Calcular", class = "btn btn-success")
#                 ),
#                 box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
#                     verbatimTextOutput("resultados_5")
#                 )
#               ),
#               fluidRow(
#                 column(12, align = "left",
#                        actionButton("anterior_5", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
#                 )
#               )
#       )
#     )
#   )
#   
# )