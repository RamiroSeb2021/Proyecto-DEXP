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

ui <-tagList(
  # 1) Inyectamos el CSS para personalizar tonos de verde
  tags$head(
    tags$style(HTML(custom_css)),
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
                         menuSubItem("Tratamientos y réplicas", tabName = "efectos"),
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
                  h3("Asignación de réplicas por variabilidad"),
                  p("Esta herramienta calcula cuántas réplicas son necesarias para cada tratamiento en un diseño experimental, sin tener en cuenta los costos, pero equilibrando la precisión de los tratamientos según su variabilidad. Los tratamientos con mayor variabilidad recibirán más réplicas."),
                  tags$ul(
                    tags$li("Cuando los costos no son una preocupación, pero se desea equilibrar la precisión, la herramienta distribuye las réplicas de forma proporcional a la variabilidad de cada tratamiento. Esto significa que los tratamientos más variables recibirán más réplicas para mejorar la precisión de los resultados."),
                    tags$li("La herramienta toma como entrada el número de tratamientos, el número total de réplicas iniciales y la desviación estándar de cada tratamiento para distribuirlas de manera eficiente.")
                  ),
                  br(),
                  p(strong("Ejemplo de aplicación:")),
                  p("Supongamos que tienes cuatro tratamientos con diferentes desviaciones estándar dadas por (6.27, 9.57, 12, 3.32) y un número total de réplicas iniciales de 5. La herramienta calculará cuántas réplicas deben asignarse a cada tratamiento, distribuyendo las réplicas de manera proporcional a la variabilidad de cada tratamiento. Los tratamientos con mayor desviación estándar recibirán más réplicas para mejorar la precisión de los resultados."),
                  p("Para mayor información accede a:", a("hola", href = "https://escuelaing.s3.amazonaws.com/production/documents/Programación_Académica_Pregrado_Periodo_2025-i.pdf?AWSAccessKeyId=AKIAWFY3NGTFJHVI634A&Signature=FNQU9BVTAGB1mt0WKOCEy2BHUMA%3D&Expires=1748480035"))
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
                  h3("Asignación de réplicas con restricción presupuestaria"),
                  p("Esta herramienta calcula cuántas réplicas son necesarias para cada tratamiento en un diseño experimental, considerando los costos por tratamiento y el presupuesto para llevarlo a cabo, con el objetivo de optimizar la precisión de los resultados dentro de un presupuesto limitado."),
                  tags$ul(
                    tags$li("Cuando el presupuesto es una restricción y los tratamientos tienen diferentes costos y niveles de variabilidad, la herramienta distribuye las réplicas de manera eficiente utilizando un enfoque basado en multiplicadores de Lagrange."),
                    tags$li("Esto permite asignar más réplicas a los tratamientos más variables teniendo en cuenta el presupuesto total disponible, maximizando así la precisión del diseño experimental."),
                    tags$li("La herramienta toma como entrada el número de tratamientos, la desviación estándar de cada tratamiento, el costo por unidad experimental de cada tratamiento y el presupuesto total disponible.")
                  ),
                  br(),
                  p(strong("Ejemplo de aplicación:")),
                  p("Supongamos que tienes cuatro tratamientos con desviaciones estándar dadas por (6.27, 9.57, 12, 3.32), los costos por tratamiento son de (1000, 200, 700, 1100) y un presupuesto total de $50.000. La herramienta calculará cuántas réplicas deben asignarse a cada tratamiento, distribuyendo los recursos disponibles de forma que se mejore la precisión teniendo en cuenta el presupuesto disponible."),
                  p("Para mayor información accede a:", a("hola", href = "https://escuelaing.s3.amazonaws.com/production/documents/Programación_Académica_Pregrado_Periodo_2025-i.pdf?AWSAccessKeyId=AKIAWFY3NGTFJHVI634A&Signature=FNQU9BVTAGB1mt0WKOCEy2BHUMA%3D&Expires=1748480035"))
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
                column(
                  width = 12,
                  h3("Asignación de tratamientos y réplicas con Función de Costos y Varianza Máxima"),
                  p("La asignación de tratamientos y réplicas en un diseño experimental se basa en un modelo de componentes de varianza, donde tanto el número de tratamientos como el número de réplicas son variables. Estos valores se ajustan de acuerdo con la necesidad de controlar las varianzas y minimizar los costos en la estimación de la media de los tratamientos. La varianza de la media muestral es una medida clave en este proceso y está determinada por las varianzas asociadas a los tratamientos y las réplicas. El desafío es encontrar los valores óptimos de tratamientos y réplicas que minimicen una función de costos dada, que incluye tanto el costo por unidad de tratamiento como el costo por unidad experimental. Este proceso matemático, descrito por Mendenhall (1968), busca la distribución eficiente de los recursos experimentales, ajustando el número de tratamientos y réplicas de manera que se mantenga constante la varianza de la media muestral, maximizando así la precisión del diseño teniendo en cuenta el presupuesto disponible."),
                  br(),
                  p(strong("Ejemplo de aplicación:")),
                  p("En este ejemplo, se busca determinar el número óptimo de toros y terneros necesarios para un experimento genético, considerando restricciones de costo y precisión. La varianza máxima permitida para la media muestral es de 43,49, lo que establece un límite sobre la variabilidad aceptable en los resultados. Además, se tienen en cuenta los costos experimentales, siendo de $150.000 por cada toro y $50.000 por cada ternero. A partir de estos valores y las estimaciones de varianza obtenidas en el ejemplo anterior (como la varianza entre toros y el error experimental), se concluye que para cumplir con las condiciones establecidas se deben seleccionar 7 toros y 3 terneros por cada toro. Este diseño permite mantener el equilibrio entre el costo total del experimento y la precisión estadística deseada.")
                ),
                p("Para mayor información accede a:", a("hola", href = "https://escuelaing.s3.amazonaws.com/production/documents/Programación_Académica_Pregrado_Periodo_2025-i.pdf?AWSAccessKeyId=AKIAWFY3NGTFJHVI634A&Signature=FNQU9BVTAGB1mt0WKOCEy2BHUMA%3D&Expires=1748480035"))

                
              ),
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
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
                    numericInput(
                      "r_potencia", 
                      label = div(
                        style = "display: inline-flex; align-items: center;",
                        "Réplicas por tratamiento (r)",
                        span(
                          class = "mi-tooltip",
                          HTML(" ⓘ"),
                          span(class = "texto-tooltip", "Aquí debes ingresar el número de réplicas por tratamiento, que debe ser un número entero positivo (ejemplo: 15). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                          style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                        )
                      ),
                      value = 15
                    ),
                    numericInput(
                      "t_potencia", 
                      label = div(
                        style = "display: inline-flex; align-items: center;",
                        "Tratamientos (t)",
                        span(
                          class = "mi-tooltip",
                          HTML(" ⓘ"),
                          span(class = "texto-tooltip", "Aquí debes ingresar el número de tratamientos, que debe ser un número entero positivo (ejemplo: 4). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                          style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                        )
                      ),
                      value = 4
                    ),
                    numericInput(
                      "sigma2_potencia", 
                      label = div(
                        style = "display: inline-flex; align-items: center;",
                        "Varianza estimada (σ²)",
                        span(
                          class = "mi-tooltip",
                          HTML(" ⓘ"),
                          span(class = "texto-tooltip", "La varianza estimada refleja la dispersión de los datos. Debe ser un número positivo (ejemplo: 10.35). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                          style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                        )
                      ),
                      value = 10.35
                    ),
                    numericInput(
                      "Delta_potencia", 
                      label = div(
                        style = "display: inline-flex; align-items: center;",
                        "Diferencia mínima detectable (Δ)",
                        span(
                          class = "mi-tooltip",
                          HTML(" ⓘ"),
                          span(class = "texto-tooltip", "Esta es la diferencia mínima entre tratamientos que se espera detectar con el experimento. Debe ser un número positivo (ejemplo: 3). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                          style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                        )
                      ),
                      value = 3
                    ),
                    numericInput(
                      "alpha_potencia", 
                      label = div(
                        style = "display: inline-flex; align-items: center;",
                        "Nivel de significancia (α)",
                        span(
                          class = "mi-tooltip",
                          HTML(" ⓘ"),
                          span(class = "texto-tooltip", "Este es el nivel de significancia para la prueba estadística, usualmente se utiliza 0.05 (ejemplo: 0.05). Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."),
                          style = "margin-left: 5px; color: #3498db; cursor: pointer;"
                        )
                      ),
                      value = 0.05
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
      )
,
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
              ),
              fluidRow(
                column(6, align = "left",
                       actionButton("anterior_6", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                ),
                column(6, align = "right",
                       actionButton("siguiente_6", "Siguiente", icon = icon("arrow-right"), class = "btn btn-success")
                )
              )
      ),
      
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