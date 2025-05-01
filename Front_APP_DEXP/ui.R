#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
source("Presentation/custom_styles.R")

ui <-tagList(
  # 1) Inyectamos el CSS para personalizar tonos de verde
  tags$head(
    tags$style(HTML(custom_css))
  ),
  dashboardPage(
  skin = "green",
  dashboardHeader(title = "Diseño Experimental",
                  # ----> aquí metemos el texto al lado del toggle
                  tags$li(
                    class = "dropdown",      # lo sitúa junto al botón de togglear sidebar
                    style = "padding: 15px; color: white; font-weight: bold;",
                    "Programa de ingeniería estadística"
                  )),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Introducción", tabName = "intro", icon = icon("info-circle")),
                menuItem("Cálculos número de réplicas", icon = icon("flask"),
                         menuSubItem("Proporcionalidad sin Costo", tabName = "sin_costo"),
                         menuSubItem("Proporcionalidad con Costo", tabName = "con_costo"),
                         menuSubItem("Efectos Aleatorios", tabName = "efectos"),
                         menuSubItem("Cálculo de Potencia", tabName = "potencia"),
                         menuSubItem("Método HHM", tabName = "hhm"),
                         menuSubItem("Método de Tukey", tabName = "metodo_tukey", icon = icon("table")),
                         menuSubItem("Simulación de Potencia", tabName = "sim_potencia", icon = icon("chart-line"))
                )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(title = "Bienvenido", width = 12, status = "primary", solidHeader = TRUE,
                    p("A continuación podrás realizar cálculos para determinar el número de réplicas en diferentes contextos de diseño experimental. Estos incluyen métodos con y sin costos, con efectos aleatorios, cálculos de potencia, y el método de Harris-Hurvitz-Mood (HHM)."),
                    p("Haz clic en 'Continuar' para comenzar con el primer cálculo: Proporcionalidad sin Costo."),
                    actionButton("continuar", "Continuar", class = "btn btn-success")
                )
              )
      ),
      tabItem(tabName = "sin_costo",
              # Estilos CSS para el tooltip (añadido en el head)
              tags$head(
                tags$style(HTML("
            .mi-tooltip {
              position: relative;
              display: inline-block;
              cursor: pointer;
            }
            .mi-tooltip .texto-tooltip {
              visibility: hidden;
              width: 160px;
              background-color: #3498db;
              color: white;
              text-align: center;
              border-radius: 6px;
              padding: 8px;
              position: absolute;
              z-index: 1000;
              bottom: 125%;
              left: 50%;
              transform: translateX(-50%);
              opacity: 0;
              transition: opacity 0.3s;
              font-size: 14px;
            }
            .mi-tooltip:hover .texto-tooltip {
              visibility: visible;
              opacity: 1;
            }
            .mi-tooltip .texto-tooltip::after {
              content: '';
              position: absolute;
              top: 100%;
              left: 50%;
              margin-left: -5px;
              border-width: 5px;
              border-style: solid;
              border-color: #3498db transparent transparent transparent;
            }
          "))
              ),
              
              fluidRow(
                column(
                  width = 12,
                  p("Esta función calcula cuántas repeticiones (tamaño de muestra) se necesitan para cada tratamiento en un experimento científico, considerando dos enfoques:"),
                  tags$ul(
                    tags$li("Cuando hay costos variables (donde cada tratamiento tiene un precio diferente), optimiza la distribución del presupuesto para minimizar el error en los resultados, asignando más repeticiones a tratamientos más variables o importantes (usando ecuaciones con multiplicadores matemáticos llamados lagrangianos)."),
                    tags$li("Cuando el costo no importa pero se quiere equilibrar la precisión, distribuye las repeticiones proporcionalmente a la variabilidad de cada tratamiento (tratamientos más variables reciben más repeticiones).")
                  ),
                  p("En esencia, es una herramienta estadística que asegura resultados confiables en experimentos, ya sea ajustándose a un presupuesto o priorizando la precisión científica.")
                )
              ),
              fluidRow(
                box(
                  title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                  numericInput(
                    inputId = "a",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Número de tratamientos",
                      span(
                        class = "mi-tooltip",
                        HTML(" ⓘ"),
                        span(class = "texto-tooltip", "Aquí debe ingresar el número de tratamientos con los que cuenta, este debe ser un número entero positivo."),
                        style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                      )
                    ),
                    value = 4
                  ),
                  # 2. Input para réplicas iniciales (con tooltip)
                  numericInput(
                    inputId = "r0",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Número de réplicas iniciales",
                      span(
                        class = "mi-tooltip",
                        HTML(" ⓘ"),
                        span(class = "texto-tooltip", "Aquí debe ingresar el número de réplicas inciales con las que cuenta, este debe ser un número entero positivo."),
                        style = "margin-left: 5px; color: #3498db; cursor: help;"
                      )
                    ),
                    value = 5,
                    width = "100%"
                  ),
                  
                  # 3. Input para desviaciones estándar (con tooltip)
                  textInput(
                    inputId = "sigmas",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Desviaciones estándar por tratamiento",
                      span(
                        class = "mi-tooltip",
                        HTML(" ⓘ"),
                        span(class = "texto-tooltip", "Aquí debe ingresar los valores de las desviaciones estándar separados por comas (ejemplo: 1.5, 2.0, 1.8). Debe asegurarse de que la cantidad de desviaciones estándar, coincida con el número de tratamientos."),
                        style = "margin-left: 5px; color: #3498db; cursor: help;"
                      )
                    ),
                    value = "6.27,9.57,12,3.32",
                    width = "100%"
                  ),
                  actionButton("calcular_1", "Calcular", class = "btn btn-success")
                ),
                box(
                  title = "Resultados", width = 6, status = "success", solidHeader = TRUE,
                  verbatimTextOutput("resultados_1")
                )
              ),
              fluidRow(
                column(12, align = "right",
                       actionButton("siguiente_1", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
                )
              )
      ),
      tabItem(tabName = "con_costo",
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("a_2", "Tratamientos (a)", 4),
                    textInput("sigmas_2", "Desviaciones σ", "6.27,9.57,12,3.32"),
                    textInput("costos", "Costos por tratamiento", "1000,200,700,1100"),
                    numericInput("costo_total", "Presupuesto total", 50000),
                    actionButton("calcular_2", "Calcular", class = "btn btn-success")
                ),
                box(title = "Resultados", width = 6, status = "success", solidHeader = TRUE,
                    verbatimTextOutput("resultados_2")
                )
              ),
              fluidRow(
                column(6, align = "left",
                       actionButton("anterior_2", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                ),
                column(6, align = "right",
                       actionButton("siguiente_2", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
                )
              )
      ),
      tabItem(tabName = "efectos",
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("costo_tratamiento", "Costo tratamiento (C₁)", 150000),
                    numericInput("costo_ue", "Costo unidad experimental (C₂)", 50000),
                    numericInput("sigma_cuadrado", "Varianza del error (σ²)", 416.21),
                    numericInput("rho", "Proporción ρ", 0.3796),
                    numericInput("v_max", "Varianza máxima (v_max)", 43.49),
                    actionButton("calcular_3", "Calcular", class = "btn btn-success")
                ),
                box(title = "Resultados", width = 6, status = "success", solidHeader = TRUE,
                    verbatimTextOutput("resultados_3")
                )
              ),
              fluidRow(
                column(6, align = "left",
                       actionButton("anterior_3", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                ),
                column(6, align = "right",
                       actionButton("siguiente_3", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
                )
              )
      ),
      tabItem(tabName = "potencia",
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("r_potencia", "Réplicas por tratamiento (r)", 15),
                    numericInput("t_potencia", "Tratamientos (t)", 4),
                    numericInput("sigma2_potencia", "Varianza estimada (σ²)", 10.35),
                    numericInput("Delta_potencia", "Diferencia mínima detectable (Δ)", 3),
                    numericInput("alpha_potencia", "Nivel de significancia (α)", 0.05),
                    actionButton("calcular_4", "Calcular", class = "btn btn-success")
                ),
                box(title = "Resultados", width = 6, status = "success", solidHeader = TRUE,
                    verbatimTextOutput("resultados_4")
                )
              ),
              fluidRow(
                column(6, align = "left",
                       actionButton("anterior_4", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                ),
                column(6, align = "right",
                       actionButton("siguiente_4", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
                )
              )
      ),
      tabItem(tabName = "hhm",
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("S1_hhm", "Desviación estándar experimental (S₁)", sqrt(141.6)),
                    numericInput("d_hhm", "Diferencia mínima detectable (d)", 20),
                    numericInput("df2_hhm", "Grados de libertad (df₂)", 60),
                    numericInput("K_hhm", "Valor K (tabla A.9)", 0.322),
                    actionButton("calcular_5", "Calcular", class = "btn btn-success")
                ),
                box(title = "Resultados", width = 6, status = "success", solidHeader = TRUE,
                    verbatimTextOutput("resultados_5")
                )
              ),
              fluidRow(
                column(12, align = "left",
                       actionButton("anterior_5", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                )
              )
      ), 
      # +1: método de Tukey
      tabItem(tabName = "metodo_tukey",
              fluidRow(
                box(
                  title = "Parámetros Método de Tukey", status = "primary", solidHeader = TRUE, width = 6,
                  numericInput("mt_T",    "Tratamientos (T_)",         value = 6, min = 2),
                  numericInput("mt_D",    "Diferencia mínima (D)",     value = 20),
                  numericInput("mt_ro",   "Réplica inicial (ro)",      value = 5, min = 1),
                  numericInput("mt_S1",   "S1 (SD experimental)",      value = sqrt(141.6)),
                  numericInput("mt_df1",  "df1 (gl de S1)",           value = 40, min = 1),
                  numericInput("mt_alfa", "Alfa",                      value = 0.05, step = 0.01),
                  numericInput("mt_Beta", "Beta (1–potencia)",         value = 0.10, step = 0.01),
                  actionButton("calcular_mt", "Calcular", class = "btn btn-success")
                ),
                box(
                  title = "Resultados Tukey", status = "success", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("resultados_mt")
                )
              )
      ),
      
      # +2: simulación de potencia / encontrar r mínimo
      tabItem(tabName = "sim_potencia",
              fluidRow(
                box(
                  title = "Parámetros Simulación", status = "primary", solidHeader = TRUE, width = 6,
                  numericInput("sim_t",            "Tratamientos (t)",               value = 5,  min = 2),
                  numericInput("sim_sigma2",       expression(sigma^2 ~ "residual"), value = 1),
                  numericInput("sim_Delta",        "Diferencia mínima (D)",         value = 20),
                  numericInput("sim_alpha",       "Alfa",                            value = 0.05, step = 0.01),
                  numericInput("sim_power_target","Potencia objetivo (1-β)",        value = 0.8,  step = 0.05),
                  numericInput("sim_r_min",       "r mínimo",                       value = 2,  min = 1),
                  numericInput("sim_r_max",       "r máximo",                       value = 50, min = 2),
                  actionButton("calcular_sim", "Calcular", class = "btn btn-success")
                ),
                box(
                  title = "Resultados Simulación", status = "success", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("resultados_sim"),
                  plotOutput("grafico_sim"),
                  DT::DTOutput("tabla_sim")
                )
              )
      )
      
    ),
    # logo fijo en la esquina inferior derecha
    tags$img(
      src = "Logo_Escuela.png",
      class = "fixed-logo",
      style = "
      position: fixed;
      bottom: 10px;
      right: 10px;
      width: 160px;   /* ajusta al tamaño que necesites */
      z-index: 1000; /* para asegurarse de que quede por encima */
    "
      )
    )
  )
)