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
source("Presentation/header_functions.R")

ui <- tagList(
  # 1) Inyectamos el CSS para personalizar tonos de verde
  tags$head(
    tags$style(HTML(custom_css)),
  ),
  dashboardPage(
    skin = "green",
    dashboardHeader(
      title = "Diseño Experimental",
      # ----> aquí metemos el texto al lado del toggle
      tags$li(
        class = "dropdown", # lo sitúa junto al botón de togglear sidebar
        style = "padding: 15px; color: white; font-weight: bold;",
        "Programa de ingeniería estadística"
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Introducción", tabName = "intro", icon = icon("info-circle")),
        menuItem("Cálculos número de réplicas",
          icon = icon("flask"),
          menuSubItem("Réplicas por variabilidad", tabName = "sin_costo"),
          menuSubItem("Réplicas con presupuesto", tabName = "con_costo"),
          menuSubItem("Tratamientos y réplicas", tabName = "efectos"),
          menuSubItem("Estimación S1 y df1", tabName = "estimacion_s1_df1"),
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
      useShinyjs(),
      useShinyFeedback(),
      tabItems(
        tabItem(
          tabName = "intro",
          fluidRow(
            box(
              title = "Bienvenido", width = 12, status = "primary", solidHeader = TRUE,
              p("A continuación podrás realizar cálculos para determinar el número de réplicas en diferentes contextos de diseño experimental. Estos incluyen métodos con y sin costos, con efectos aleatorios, cálculos de potencia, y el método de Harris-Hurvitz-Mood (HHM)."),
              p("Haz clic en 'Continuar' para comenzar con el primer cálculo: Réplicas por variabilidad."),
              actionButton("continuar", "Continuar", class = "btn btn-success")
            )
          )
        ),

        # Asignación de réplicas por variabilidad ---------------------------------


        tabItem(
          tabName = "sin_costo",
          # Estilos CSS para el tooltip (añadido en el head)
          tags$head(
            tags$style(HTML(desc_sinCosto))
          ),
          fluidRow(
            column(
              width = 12,
              h3("Asignación de réplicas por variabilidad"),
              p("Esta herramienta calcula cuántas réplicas son necesarias para cada tratamiento en un diseño experimental, sin tener en cuenta los costos, pero equilibrando la precisión de los tratamientos según su variabilidad. Los tratamientos con mayor variabilidad recibirán más réplicas."),
              p("Para mayor información accede a:", a("Info dexp app", href = "https://rpubs.com/juanayaramiro/1312041"))
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
                    class = "tooltip-right",
                    HTML(" ⓘ"),
                    span(class = "tooltip-right-content", "Aquí debes ingresar el número de tratamientos con los que cuentas, este debe ser un número entero positivo (ejemplo:4). Debes asegurarte de que la cantidad de tratamientos, coincida con el número de desviaciones estándar."),
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
                    style = "
                        margin-left: 5px;
                        color: #3498db;
                        cursor: help;
                        /* Posicionamiento para tooltip arriba */
                        position: relative;
                        display: inline-block;
                      "
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
            column(12,
              align = "right",
              actionButton("siguiente_1", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
            )
          )
        ),

        # Asignación de réplicas con restricción presupuestaria -------------------


        tabItem(
          tabName = "con_costo",
          # Estilos CSS para el tooltip (añadido en el head)
          tags$head(
            tags$style(HTML(desc_conCosto))
          ),
          fluidRow(
            column(
              width = 12,
              h3("Asignación de réplicas con restricción presupuestaria"),
              p("Esta herramienta calcula cuántas réplicas son necesarias para cada tratamiento en un diseño experimental, considerando los costos por tratamiento y el presupuesto para llevarlo a cabo, con el objetivo de optimizar la precisión de los resultados dentro de un presupuesto limitado."),
              p("Para mayor información accede a:", a("Info app dexp", href = "https://rpubs.com/juanayaramiro/1312041"))
            )
          ),
          fluidRow(
            box(
              title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
              # NumericInput con tooltip para tratamientos
              numericInput(
                inputId = "a_2",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Número de tratamientos",
                  span(
                    class = "tooltip-right",
                    HTML(" ⓘ"),
                    span(class = "tooltip-right-content", "Aquí debes ingresar el número de tratamientos con los que cuentas, este debe ser un número entero positivo (ejemplo:4). Debes asegurarte de que la cantidad de tratamientos, coincida con el número de desviaciones estándar y con el número de costos."),
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
            box(
              title = "Resultados", width = 6, status = "success", solidHeader = TRUE,
              verbatimTextOutput("resultados_2")
            )
          ),
          fluidRow(
            column(6,
              align = "left",
              actionButton("anterior_2", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
            ),
            column(6,
              align = "right",
              actionButton("siguiente_2", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
            )
          )
        ),
        # Asignación de tratamientos y réplicas con Función de Costos y Va --------


        tabItem(
          tabName = "efectos",
          fluidRow(
            column(
              width = 12,
              h3("Asignación de tratamientos y réplicas con Función de Costos y Varianza Máxima"),
              p("La asignación de tratamientos y réplicas en un diseño experimental se basa en un modelo de componentes de varianza, donde tanto el número de tratamientos como el número de réplicas son variables. Estos valores se ajustan de acuerdo con la necesidad de controlar las varianzas y minimizar los costos en la estimación de la media de los tratamientos. La varianza de la media muestral es una medida clave en este proceso y está determinada por las varianzas asociadas a los tratamientos y las réplicas. El desafío es encontrar los valores óptimos de tratamientos y réplicas que minimicen una función de costos dada, que incluye tanto el costo por unidad de tratamiento como el costo por unidad experimental. Este proceso matemático, descrito por Mendenhall (1968), busca la distribución eficiente de los recursos experimentales, ajustando el número de tratamientos y réplicas de manera que se mantenga constante la varianza de la media muestral, maximizando así la precisión del diseño teniendo en cuenta el presupuesto disponible."),
              br(),
              p("Para mayor información accede a:", a("Info app dexp", href = "https://rpubs.com/juanayaramiro/1312041"))
            ),
          ),
          fluidRow(
            box(
              title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
              numericInput(
                "costo_tratamiento",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Costo por unidad de tratamiento (C₁)",
                  span(
                    class = "mi-tooltip",
                    HTML(" ⓘ"),
                    span(class = "texto-tooltip", "Este es el costo de cada tratamiento, debe ser un número positivo mayor que cero (ejemplo: 150000). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                    style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                  )
                ),
                value = 150000
              ),
              numericInput(
                "costo_ue",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Costo por unidad experimental (C₂)",
                  span(
                    class = "mi-tooltip",
                    HTML(" ⓘ"),
                    span(class = "texto-tooltip", "Este es el costo de cada unidad experimental, debe ser un número positivo mayor que cero (ejemplo: 50000). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                    style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                  )
                ),
                value = 50000
              ),
              numericInput(
                "sigma_cuadrado",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Varianza dentro de los tratamientos",
                  span(
                    class = "mi-tooltip",
                    HTML(" ⓘ"),
                    span(class = "texto-tooltip", "La varianza σ² refleja la dispersión dentro de los tratamientos. Debe ser un número positivo (ejemplo: 416.21). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                    style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                  )
                ),
                value = 416.21
              ),
              numericInput(
                "rho",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Proporción de la varianza total",
                  span(
                    class = "mi-tooltip",
                    HTML(" ⓘ"),
                    span(class = "texto-tooltip", "Este parámetro representa la proporción de la varianza total atribuida a los tratamientos. Debe estar entre 0 y 1 (ejemplo: 0.3796). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                    style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                  )
                ),
                value = 0.3796
              ),
              numericInput(
                "v_max",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Varianza máxima tolerable para la media muestral",
                  span(
                    class = "mi-tooltip",
                    HTML(" ⓘ"),
                    span(class = "texto-tooltip", "Este es el valor máximo aceptable para la varianza de la media muestral. Debe ser un número positivo mayor que cero (ejemplo: 43.49). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                    style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                  )
                ),
                value = 43.49
              ),
              actionButton("calcular_3", "Calcular", class = "btn btn-success")
            ),
            box(
              title = "Resultados", width = 6, status = "success", solidHeader = TRUE,
              verbatimTextOutput("resultados_3")
            )
          ),
          fluidRow(
            column(6,
              align = "left",
              actionButton("anterior_3", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
            ),
            column(6,
              align = "right",
              actionButton("siguiente_3", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
            )
          )
        ),
        

# Estimacion varianza -----------------------------------------------------

        
        tabItem(
          tabName = "estimacion_s1_df1",
          
          fluidRow(
            column(
              width = 12,
              h3("Estimación de S1 y df1"),
              p("Esta herramienta permite estimar el valor desviación estándar (S1) y los grados de libertad (df1) necesarios para construir un intervalo de confianza en torno a la media estimada, usando márgenes relativos y distribuciones chi-cuadrado. Lo anterior 
                llegado el caso que no se tenga información previa de esta."),
              p("Para mayor información accede a:", a("Info app dexp", href = "https://rpubs.com/juanayaramiro/1312041"))
            )
          ),
          
          fluidRow(
            box(
              title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
              
              numericInput(
                inputId = "s1_est_sd",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Desviación estándar estimada",
                  span(
                    class = "mi-tooltip", HTML(" ⓘ"),
                    span(class = "texto-tooltip", "Desviación estandar de la media, esta debe de ser myor a cero"),
                    style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                  )
                ),
                value = 30, min = 0
              ),
              
              numericInput(
                inputId = "s1_est_Si",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Porcentaje inferior relativo (Si)",
                  span(
                    class = "mi-tooltip", HTML(" ⓘ"),
                    span(class = "texto-tooltip", "Porentaje el cual se desea que se desvíe por abajo la desviación estandar. Debe de ser un valor entre cero y uno"),
                    style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                  )
                ),
                value = 0.07, min = 0
              ),
              
              numericInput(
                inputId = "s1_est_Ss",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Porcentaje superior relativo (Ss)",
                  span(
                    class = "mi-tooltip", HTML(" ⓘ"),
                    span(class = "texto-tooltip", "Porentaje el cual se desea que se desvíe por arriba la desviación estandar. Debe de ser un valor entre cero y uno"),
                    style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                  )
                ),
                value = 0.12, min = 0
              ),
              
              actionButton("calcular_s1_df1", "Calcular", class = "btn btn-success")
            ),
            
            box(
              title = "Resultados", width = 6, status = "success", solidHeader = TRUE,
              verbatimTextOutput("resultado_s1_df1")
            )
          ),
          fluidRow(
            column(6,
                   align = "left",
                   actionButton("anterior_3.5", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
            ),
            column(6,
                   align = "right",
                   actionButton("siguiente_3.5", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
            )
          )
        )
        ,
        

        # POTENCIA ----------------------------------------------------------------
        tabItem(
          tabName = "potencia",
          fluidRow(
            column(
              width = 12,
              h3("Cálculo del tamaño muestral a partir de la potencia"),
              p(head_potencia),
              p("Para mayor información accede a:", a("Info dexp app", href = "https://rpubs.com/juanayaramiro/1312041"))
            )
          ),
          fluidRow(
            # tus parámetros siguen igual…
            box(
              title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
              ## t: Tratamientos
              numericInput(
                inputId = "t_potencia",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Tratamientos (t)",
                  span(
                    class = "tooltip-right",
                    HTML(" ⓘ"),
                    span(
                      class = "tooltip-right-content",
                      tratamiento_message_pot
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
              numericInput(
                inputId = "pot_rho",
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
                value = 0.5, min = 0, step = 0.01
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
            # Aquí reemplazamos el box de resultados por un tabBox de 2 pestañas
            # en tu UI, pon el tabBox así (fijo, sin renderUI)
            shinydashboard::tabBox(
              title = "Resultados Simulación",
              id    = "pot_res_tabs",
              width = 6,
              
              tabPanel("Gráfico",
                       # Loading placeholder para el gráfico
                       hidden(
                         div(
                           id    = "loading_pot_plot",
                           style = "text-align:center; padding:20px;",
                           img(src = "loading.gif", height = "100px"),
                           p("Calculando gráfico…")
                         )),
                       # Contenedor del plot, inicialmente oculto
                       hidden(
                         div(
                           id = "plot_pot_container",
                           plotOutput("grafico_pot", height = "400px"),
                           br(),
                           div(style="padding: 8px;",
                               textOutput("mensaje_pot"))
                           
                         )
                       )
              ),
              
              tabPanel("Tabla",
                       # Loading placeholder para la tabla
                       hidden(
                         div(
                           id    = "loading_pot_table",
                           style = "text-align:center; padding:20px;",
                           img(src = "loading.gif", height = "100px"),
                           p("Calculando tabla…")
                         )),
                       # Contenedor de la DT, inicialmente oculto
                       hidden(
                         div(
                           id = "table_pot_container",
                           DT::DTOutput("tabla_pot")
                         )
                       )
              )
            )
            
          ),
          fluidRow(
            column(6,
                   align = "left",
                   actionButton("anterior_4", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
            ),
            column(6,
                   align = "right",
                   actionButton("siguiente_4", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
            )
          )
        ),

        # METODO HHM ----------------------------------------------------------


        tabItem(
          tabName = "hhm",
          fluidRow(
            column(
              width = 12,
              h3("Método de Harris–Hurvitz–Mood (HHM)"),
              p(head_HHM),
              p("Para mayor información accede a:", a("Info dexp app", href = "https://rpubs.com/juanayaramiro/1312041"))
            )
          ),
          fluidRow(
            box(
              title = "Parámetros HHM", width = 6, status = "primary", solidHeader = TRUE,
              
              ## t: Tratamientos
              numericInput(
                inputId = "t_hhm",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Tratamientos (t)",
                  span(
                    class = "tooltip-right",
                    HTML(" ⓘ"),
                    span(
                      class = "tooltip-right-content",
                      tratamiento_message_pot
                    ),
                    style = "margin-left: 5px; color: #3498db; cursor: help;"
                  )
                ),
                value = 6, min = 2
              ),

              ## S2₁: Varianza estimada grupo 1
              numericInput(
                inputId = "S2_1_hhm",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Varianza estimada S1", tags$sup("2"),
                  span(
                    class = "tooltip-right",
                    HTML(" ⓘ"),
                    span(
                      class = "tooltip-right-content",
                      "Aquí debes ingresar la varianza estimada del grupo 1. Si es decimal, sepáralo con coma (ejemplo: 141,6)."
                    ),
                    style = "margin-left: 5px; color: #3498db; cursor: help;"
                  )
                ),
                value = 141.6
              ),
              
              ## df1: Grados de libertad de S1
              numericInput(
                inputId = "df2_hhm",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Grados de libertad df1",
                  span(
                    class = "mi-tooltip",
                    HTML(" ⓘ"),
                    span(
                      class = "texto-tooltip",
                      "Aquí debes ingresar los grados de libertad de S1, entero ≥ 1 (ejemplo: 60)."
                    ),
                    style = "margin-left: 5px; color: #3498db; cursor: help;"
                  )
                ),
                value = 40, min = 1
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
              numericInput(
                inputId = "hhm_ro",
                label = div(
                  style = "display: inline-flex; align-items: center;",
                  "Tamaño de muestra inicial",
                  span(
                    class = "mi-tooltip", HTML(" ⓘ"),
                    span(class = "texto-tooltip", r_0),
                    style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                  )
                ),
                value = 3, min = 1
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
              ## beta: Error tipo II (potencia objetivo)
              numericInput(
                inputId = "beta_potencia_HHM",
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
              actionButton("calcular_5", "Calcular", class = "btn btn-success")
            ),
            box(
              title = "Resultados HHM", width = 6, status = "success", solidHeader = TRUE,
              # GIF de carga (oculto al inicio)
              hidden(
                div(
                  id = "loading_hhm",
                  style = "text-align: center; padding: 20px;",
                  img(src = "loading.gif", height = "80px"),
                  p("Calculando...")
                )
              ),
              # Contenedor de resultados (oculto al inicio)
              hidden(
                div(
                  id = "hhm_results_container",
                  verbatimTextOutput("resultados_5")
                )
              )
            )
          ),
          fluidRow(
            column(6,
              align = "left",
              actionButton("anterior_5", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
            ),
            column(6,
              align = "right",
              actionButton("siguiente_5", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
            )
          )
        ),

        # METODO DE TUKEY ------------------------------------------------------------


        tabItem(
          tabName = "metodo_tukey",

          # 🔹 Descripción del método (fila completa)
          fluidRow(
            column(
              width = 12,
              h3("Cálculo del tamaño muestral - Método de Tukey"),
              p("Esta herramienta permite estimar el número de réplicas necesarias por tratamiento en un diseño experimental, utilizando el método de Tukey para comparaciones múltiples."),
              tags$ul(
                tags$li("El objetivo es garantizar que, al aplicar pruebas post-hoc entre tratamientos, sea posible detectar una diferencia mínima significativa previamente establecida en la variable de interés."),
                tags$li("Si dicha diferencia no se detecta en el análisis, se asume que el tamaño muestral inicial no fue suficiente para evidenciarla estadísticamente.")
              ),
              p("Para mayor información accede a:", a("Info dexp app", href = "https://rpubs.com/juanayaramiro/1312041"))
            )
          ),
          fluidRow(
            # Panel de parámetros
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
                value = 6, min = 2
              ),
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
                  "Grados de libertad (entre grupos)",
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
            ),

            # Panel de resultados
            box(
              title = "Resultados Método de Tukey", status = "success", solidHeader = TRUE, width = 6,
              verbatimTextOutput("resultados_mt")
            )
          ),

          # Botones de navegación
          fluidRow(
            column(6,
              align = "left",
              actionButton("anterior_6", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
            ),
            column(6,
              align = "right",
              actionButton("siguiente_6", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
            )
          )
        ),

        # SIMULACION DE POTENCIA --------------------------------------------------

        tabItem(
          tabName = "sim_potencia",
          fluidRow(
            column(
              width = 12,
              h3("Número de réplicas para modelos de efectos aleatorios"),
              p("Calcula el número de réplicas necesarias por tratamiento en un diseño experimental con efectos aleatorios,
              asegurando una potencia adecuada para detectar diferencias entre niveles del factor."),
              p("Utiliza curvas características de operación (OC) para evaluar la probabilidad de error tipo II según la configuración del diseño."),
              p("Para mayor información accede a:", a("Info dexp app", href = "https://rpubs.com/juanayaramiro/1312041"))
            )
          ),
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
            ),
            # Aquí reemplazamos el box de resultados por un tabBox de 2 pestañas
            # en tu UI, pon el tabBox así (fijo, sin renderUI)
            shinydashboard::tabBox(
              title = "Resultados Simulación",
              id    = "sim_res_tabs",
              width = 6,
              
              tabPanel("Gráfico",
                       # Loading placeholder para el gráfico
                       hidden(
                         div(
                           id    = "loading_sim_plot",
                           style = "text-align:center; padding:20px;",
                           img(src = "loading.gif", height = "100px"),
                           p("Calculando gráfico…")
                       )),
                       # Contenedor del plot, inicialmente oculto
                       hidden(
                         div(
                           id = "plot_sim_container",
                           plotOutput("grafico_sim", height = "400px"),
                           br(),
                           div(style="padding: 8px;",
                               textOutput("mensaje_sim"))

                         )
                       )
              ),
              
              tabPanel("Tabla",
                       # Loading placeholder para la tabla
                       hidden(
                         div(
                           id    = "loading_sim_table",
                           style = "text-align:center; padding:20px;",
                           img(src = "loading.gif", height = "100px"),
                           p("Calculando tabla…")
                       )),
                       # Contenedor de la DT, inicialmente oculto
                       hidden(
                         div(
                           id = "table_sim_container",
                           DT::DTOutput("tabla_sim")
                         )
                       )
              )
            )
            
          ),
          fluidRow(
            column(12,
              align = "left",
              actionButton("anterior_7", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
            )
          )
        )
      )
    )
  )
)
