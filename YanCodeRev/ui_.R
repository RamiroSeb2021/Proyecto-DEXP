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

ui <- tagList(
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

# POTENCIA YAN ----------------------------------------------------------------

        
        tabItem(tabName = "potencia",
                fluidRow(
                  box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                      ## t: Tratamientos
                      numericInput(
                        inputId = "t_potencia",
                        label = div(
                          style = "display: inline-flex; align-items: center;",
                          "Tratamientos (t)",
                          span(
                            class = "mi-tooltip",
                            HTML(" ⓘ"),
                            span(
                              class = "texto-tooltip",
                              "Aquí debes ingresar el número de niveles de tratamiento, debe ser entero ≥ 2 (ejemplo: 4)."
                            ),
                            style = "margin-left: 5px; color: #3498db; cursor: help;"
                          )
                        ),
                        value = 4, min = 2
                      ),
                      
                      ## sigma2: Varianza estimada
                      numericInput(
                        inputId = "sigma2_potencia",
                        label = div(
                          style = "display: inline-flex; align-items: center;",
                          "Varianza estimada (σ²)",
                          span(
                            class = "mi-tooltip",
                            HTML(" ⓘ"),
                            span(
                              class = "texto-tooltip",
                              "Aquí debes ingresar tu estimación de varianza residual. Si es decimal, sepáralo con coma (ejemplo: 10,35)."
                            ),
                            style = "margin-left: 5px; color: #3498db; cursor: help;"
                          )
                        ),
                        value = 10.35, min = 0
                      ),
                      
                      ## Delta: Diferencia mínima detectable
                      numericInput(
                        inputId = "Delta_potencia",
                        label = div(
                          style = "display: inline-flex; align-items: center;",
                          "Diferencia mínima detectable (Δ)",
                          span(
                            class = "mi-tooltip",
                            HTML(" ⓘ"),
                            span(
                              class = "texto-tooltip",
                              "Aquí debes ingresar la mínima diferencia que quieres detectar. Si es decimal, sepáralo con coma (ejemplo: 3,0)."
                            ),
                            style = "margin-left: 5px; color: #3498db; cursor: help;"
                          )
                        ),
                        value = 3, min = 0
                      ),
                      
                      ## alpha: Nivel de significancia
                      numericInput(
                        inputId = "alpha_potencia",
                        label = div(
                          style = "display: inline-flex; align-items: center;",
                          "Nivel de significancia (α)",
                          span(
                            class = "mi-tooltip",
                            HTML(" ⓘ"),
                            span(
                              class = "texto-tooltip",
                              "Aquí debes ingresar el nivel de α, valor entre 0 y 1. Si es decimal, sepáralo con coma (ejemplo: 0,05)."
                            ),
                            style = "margin-left: 5px; color: #3498db; cursor: help;"
                          )
                        ),
                        value = 0.05, min = 0, max = 1
                      ),
                      
                      ## beta: Error tipo II (potencia objetivo)
                      numericInput(
                        inputId = "beta_potencia",
                        label = div(
                          style = "display: inline-flex; align-items: center;",
                          "Potencia objetivo (1−β)",
                          span(
                            class = "mi-tooltip",
                            HTML(" ⓘ"),
                            span(
                              class = "texto-tooltip",
                              "Aquí debes ingresar la potencia deseada, valor entre 0 y 1. Si es decimal, sepáralo con coma (ejemplo: 0,80)."
                            ),
                            style = "margin-left: 5px; color: #3498db; cursor: help;"
                          )
                        ),
                        value = 0.80, min = 0, max = 1
                      ),
                      
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
        

# METODO HHM YAN ----------------------------------------------------------

        
        tabItem(tabName = "hhm",
                fluidRow(
                  box(title = "Parámetros HHM", width = 6, status = "primary", solidHeader = TRUE,
                      
                      ## S2₁: Varianza estimada grupo 1
                      numericInput(
                        inputId = "S2_1_hhm",
                        label = div(
                          style = "display: inline-flex; align-items: center;",
                          "Varianza estimada S2₁ (g²)",
                          span(
                            class = "mi-tooltip",
                            HTML(" ⓘ"),
                            span(
                              class = "texto-tooltip",
                              "Aquí debes ingresar la varianza estimada del grupo 1. Si es decimal, sepáralo con coma (ejemplo: 141,6)."
                            ),
                            style = "margin-left: 5px; color: #3498db; cursor: help;"
                          )
                        ),
                        value = 141.6
                      ),
                      
                      ## d: Diferencia mínima detectable
                      numericInput(
                        inputId = "d_hhm",
                        label = div(
                          style = "display: inline-flex; align-items: center;",
                          "Diferencia mínima detectable (d)",
                          span(
                            class = "mi-tooltip",
                            HTML(" ⓘ"),
                            span(
                              class = "texto-tooltip",
                              "Aquí debes ingresar la diferencia mínima a detectar. Si es decimal, sepáralo con coma (ejemplo: 20,5)."
                            ),
                            style = "margin-left: 5px; color: #3498db; cursor: help;"
                          )
                        ),
                        value = 20
                      ),
                      
                      ## df2: Grados de libertad df₂
                      numericInput(
                        inputId = "df2_hhm",
                        label = div(
                          style = "display: inline-flex; align-items: center;",
                          "Grados de libertad df₂",
                          span(
                            class = "mi-tooltip",
                            HTML(" ⓘ"),
                            span(
                              class = "texto-tooltip",
                              "Aquí debes ingresar los grados de libertad del error, entero ≥ 1 (ejemplo: 60)."
                            ),
                            style = "margin-left: 5px; color: #3498db; cursor: help;"
                          )
                        ),
                        value = 60, min = 1
                      ),
                      
                      ## alpha: Nivel de significancia
                      numericInput(
                        inputId = "alpha_hhm",
                        label = div(
                          style = "display: inline-flex; align-items: center;",
                          "Nivel de significancia (α)",
                          span(
                            class = "mi-tooltip",
                            HTML(" ⓘ"),
                            span(
                              class = "texto-tooltip",
                              "Aquí debes ingresar el nivel de α, valor entre 0 y 1. Si es decimal, sepáralo con coma (ejemplo: 0,05)."
                            ),
                            style = "margin-left: 5px; color: #3498db; cursor: help;"
                          )
                        ),
                        value = 0.05, min = 0, max = 1
                      ),
                      
                      actionButton("calcular_5", "Calcular", class = "btn btn-success")
                  ),
                  box(title = "Resultados HHM", width = 6, status = "success", solidHeader = TRUE,
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


# otros metodos -----------------------------------------------------------


        
        # +2: simulación de potencia / encontrar r mínimo
        tabItem(tabName = "sim_potencia",
                fluidRow(
                  # tus parámetros siguen igual…
                  box(
                    title = "Parámetros Simulación", status = "primary", solidHeader = TRUE, width = 6,
                    numericInput("sim_t", "Tratamientos (t)", value = 5,  min = 2),
                    numericInput("sim_rho", "Proporcion varianzas", value = 0.4,  min = 2),
                    numericInput("sim_sigma2",       expression(sigma^2 ~ "residual"), value = 1),
                    numericInput("sim_alpha", "Alfa",value = 0.05, step = 0.01),
                    numericInput("sim_power_target", "Potencia objetivo (1-β)", value = 0.8,  step = 0.05),
                    numericInput("sim_r_max", "Tamaño maximo de tamaño de muestra", value = 50,  min = 1),
                    actionButton("calcular_sim", "Calcular", class = "btn btn-success")
                  ),
                  
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
