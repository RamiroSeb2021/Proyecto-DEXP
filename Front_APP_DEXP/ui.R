#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shinyjs)
library(shiny)
library(shinyWidgets)
library(shinyFeedback)
library(shinydashboard)
library(dplyr)
source("Presentation/custom_styles.R")
source("Presentation/function_description.R")
source("Code/message_tooltips.R")

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
                         menuSubItem("Réplicas por variabilidad", tabName = "sin_costo"),
                         menuSubItem("Réplicas con presupuesto", tabName = "con_costo"),
                         menuSubItem("Efectos Aleatorios", tabName = "efectos"),
                         menuSubItem("Cálculo de Potencia", tabName = "potencia"),
                         menuSubItem("Método HHM", tabName = "hhm"),
                         menuSubItem("Método de Tukey", tabName = "metodo_tukey"),
                         menuSubItem("Simulación de Potencia", tabName = "sim_potencia")
                )
    ),
    # aquí insertamos el logo al fondo
    tags$div(
      style = "position: absolute; bottom: 0; width: 100%; text-align: center; padding: 10px;",
      img(
        src   = "Logo_Escuela_sin_fondo.png",
        style = "max-width: 98%; height: auto; display: block; margin: 0 auto;"
      )
    )
  ),
  dashboardBody(
    useShinyjs(),       # <<--- aquí
    useShinyFeedback(), # <<--- aquí
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
                tags$style(HTML(desc_sinCosto))
              ),
              
              fluidRow(
                column(
                  width = 12,
                  h3("Asignación de réplicas por variabilidad"),  # Título para "sin costo"
                )
              )
              ,
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
                        span(class = "texto-tooltip", "Aquí debes ingresar el número de tratamientos con los que cuentas, este debe ser un número entero positivo (ejemplo:4). Debes asegurarte de que la cantidad de tratamientos, coincida con el número de desviaciones estándar."),
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
                        span(class = "texto-tooltip", "Aquí debes ingresar el número de réplicas inciales con las que cuentas, este debe ser un número entero positivo (ejemplo:5)."),
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
                        span(class = "texto-tooltip", "Aquí debes ingresar los valores de las desviaciones estándar con los que cuentas, separados por comas (ejemplo: 6.27, 9.57, 12, 3.32). Debes asegurarte de que la cantidad de desviaciones estándar, coincida con el número de tratamientos."),
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
              # Estilos CSS para el tooltip (añadido en el head)
              tags$head(
                tags$style(HTML(desc_conCosto))
              ),
              fluidRow(
                column(
                  width = 12,
                  h3("Asignación de réplicas con restricción presupuestaria"),  # Título para "con costo"
                )
              )
              
              ,
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                    # NumericInput con tooltip para tratamientos
                    numericInput(
                      inputId = "a_2",
                      label = div(
                        style = "display: inline-flex; align-items: center;",
                        "Número de tratamientos",
                        span(
                          class = "mi-tooltip",
                          HTML(" ⓘ"),
                          span(class = "texto-tooltip", "Aquí debes ingresar el número de tratamientos con los que cuentas, este debe ser un número entero positivo (ejemplo:4). Debes asegurarte de que la cantidad de tratamientos, coincida con el número de desviaciones estándar y con el número de costos."),
                          style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                        )
                      ),
                      value = 4
                    ),
                    # TextInput con tooltip para desviaciones σ
                    textInput(
                      inputId = "sigmas_2",
                      label = div(
                        style = "display: inline-flex; align-items: center;",
                        "Desviaciones estándar por tratamiento",
                        span(
                          class = "mi-tooltip",
                          HTML(" ⓘ"),
                          span(class = "texto-tooltip", "Aquí debes ingresar los valores de las desviaciones estándar con los que cuentas, separados por comas (ejemplo: 6.27, 9.57, 12, 3.32). Debes asegurarte de que la cantidad de desviaciones estándar, coincida con el número de tratamientos y con el número de costos."),
                          style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                        )
                      ),
                      value = "6.27,9.57,12,3.32"
                    ),
                    # TextInput con tooltip para costos
                    textInput(
                      inputId = "costos",
                      label = div(
                        style = "display: inline-flex; align-items: center;",
                        "Costos por unidad experimental de cada tratamiento",
                        span(
                          class = "mi-tooltip",
                          HTML(" ⓘ"),
                          span(class = "texto-tooltip", "Aquí debes ingresar los costos correspondientes a una unidad experimental de cada tratamiento con los que cuentas, separados por comas (ejemplo: 1000, 200, 700, 1100). Debes asegurarte de que la cantidad de costos, coincida con el número de desviaciones estándar y con el número de tratamientos."),
                          style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                        )
                      ),
                      value = "1000,200,700,1100"
                    ),
                    # NumericInput con tooltip para presupuesto total
                    numericInput(
                      inputId = "costo_total",
                      label = div(
                        style = "display: inline-flex; align-items: center;",
                        "Presupuesto total",
                        span(
                          class = "mi-tooltip",
                          HTML(" ⓘ"),
                          span(class = "texto-tooltip", "Aquí debes ingresar el presupuesto total disponible para el experimento con el que cuentas, sin utilizar signo pesos, puntos ni comas, solo el número (ejemplo: 50000)."),
                          style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                        )
                      ),
                      value = 50000
                    ),
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
      )
      ,
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
                column(6, align = "left",
                       actionButton("anterior_5", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                ),
                column(6, align = "right",
                       actionButton("siguiente_5", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
                )
              )
      ), 

# metodo tukey ------------------------------------------------------------


      tabItem(tabName = "metodo_tukey",
              fluidRow(
                box(
                  title = "Parámetros Método de Tukey", status = "primary", solidHeader = TRUE, width = 6,
                  
                  numericInput(
                    inputId = "mt_T",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Número de tratamientos ",
                      span(
                        class = "tooltip-right", HTML(" ⓘ"),
                        span(
                          class = "tooltip-right-content",
                          Tratamientos
                        )
                      )
                    ),
                    value = 6,
                    min = 2
                  )
                  ,
                  
                  numericInput(
                    inputId = "mt_D",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Diferencia mínima a detectar",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(class = "texto-tooltip", DifMin),
                        style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                      )
                    ),
                    value = 20
                  ),
                  
                  numericInput(
                    inputId = "mt_ro",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Tamaño de muestra inicial",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(class = "texto-tooltip", r_0),
                        style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                      )
                    ),
                    value = 5, min = 1
                  ),
                  
                  numericInput(
                    inputId = "mt_S1",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Desviación estándar",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(class = "texto-tooltip", S1_),
                        style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                      )
                    ),
                    value = sqrt(141.6)
                  ),
                  
                  numericInput(
                    inputId = "mt_df1",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Grados de libertad del tratamiento (entre grupos)",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(class = "texto-tooltip", df1_),
                        style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                      )
                    ),
                    value = 40, min = 1
                  ),
                  
                  numericInput(
                    inputId = "mt_alfa",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Nivel de significancia (α)",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(class = "texto-tooltip", alpha),
                        style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                      )
                    ),
                    value = 0.05, step = 0.01
                  ),
                  
                  numericInput(
                    inputId = "mt_Beta",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Potencia objetivo (1 − β)",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(class = "texto-tooltip", potencia),
                        style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                      )
                    ),
                    value = 0.10, step = 0.01
                  ),
                  
                  actionButton("calcular_mt", "Calcular", class = "btn btn-success")
                )
              )
      ),
      
      # +2: simulación de potencia / encontrar r mínimo
      tabItem(tabName = "sim_potencia",
              fluidRow(
                # tus parámetros siguen igual…
                box(
                  title = "Parámetros Simulación", status = "primary", solidHeader = TRUE, width = 6,
                  
                  numericInput(
                    inputId = "sim_t",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Número de tratamientos ",
                      span(
                        class = "tooltip-right", HTML(" ⓘ"),
                        span(
                          class = "tooltip-right-content",
                          Tratamientos
                        )
                      )
                    ),
                    value = 5,
                    min = 2
                  ),
                  
                  numericInput(
                    inputId = "sim_rho",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Suerte de cociente (ρ)",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(
                          class = "texto-tooltip",
                          rho_
                        ),
                        style = "margin-left: 5px; color: #3498db; cursor: help;"
                      )
                    ),
                    value = 0.4, min = 0, step = 0.01
                  ),
                  
                  numericInput(
                    inputId = "sim_sigma2",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      expression("Varianza del error σ²."),
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(
                          class = "texto-tooltip",
                          var_2_
                        ),
                        style = "margin-left: 5px; color: #3498db; cursor: help;"
                      )
                    ),
                    value = 1, min = 0
                  ),
                  
                  numericInput(
                    inputId = "sim_alpha",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Nivel de significancia (α)",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(
                          class = "texto-tooltip",
                          alpha
                        ),
                        style = "margin-left: 5px; color: #3498db; cursor: help;"
                      )
                    ),
                    value = 0.05, step = 0.01, min = 0, max = 1
                  ),
                  
                  numericInput(
                    inputId = "sim_power_target",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Potencia objetivo (1-β)",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(
                          class = "texto-tooltip",
                          potencia
                        ),
                        style = "margin-left: 5px; color: #3498db; cursor: help;"
                      )
                    ),
                    value = 0.8, step = 0.05, min = 0, max = 1
                  ),
                  
                  numericInput(
                    inputId = "sim_r_max",
                    label = div(
                      style = "display: inline-flex; align-items: center;",
                      "Número máximo de réplicas que se probarán en la simulación.",
                      span(
                        class = "mi-tooltip", HTML(" ⓘ"),
                        span(
                          class = "texto-tooltip",
                          r_max_sim
                        ),
                        style = "margin-left: 5px; color: #3498db; cursor: help;"
                      )
                    ),
                    value = 50, min = 1
                  ),
                  
                  actionButton("calcular_sim", "Calcular", class = "btn btn-success")
                )
                ,
                
                # Aquí reemplazamos el box de resultados por un tabBox de 2 pestañas
                shinydashboard::tabBox(
                  title = "Resultados Simulación",
                  id    = "sim_res_tabs",
                  width = 6,
                  # pestaña 1: Gráfico
                  tabPanel("Gráfico",
                           plotOutput("grafico_sim", height = "400px"),
                           br(),
                           div(style="padding: 8px;",
                               textOutput("mensaje_sim")
                           )
                  ),
                  # pestaña 2: Tabla
                  tabPanel("Tabla",
                           DT::DTOutput("tabla_sim")
                  )
                )
              ),
              
              fluidRow(
                column(12, align = "left",
                       actionButton("anterior_7", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                )
              )
      )
      
    )
    )
  )
)