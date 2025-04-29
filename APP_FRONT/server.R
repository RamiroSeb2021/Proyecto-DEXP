library(shiny)

# Carga de funciones
source("calculo_potencia_anova.R")  # contiene calcular_potencia y encontrar_r_minimo
source("calculo_HHM.R")                # contiene calcular_r_HHM

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Calculadora Estadística: ANOVA y HHM"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Potencia ANOVA",
                 numericInput("t", "Número de tratamientos", value = 4, min = 2),
                 numericInput("Delta", "Δ (diferencia mínima)", value = 3),
                 numericInput("sigma2", "σ² (varianza)", value = 10.35),
                 numericInput("alpha", "Nivel α", value = 0.05, min = 0, max = 1, step = 0.01),
                 numericInput("r", "Réplicas r", value = 15, min = 1),
                 actionButton("calc_power", "Calcular potencia")
        ),
        tabPanel("Tamaño de muestra ANOVA",
                 numericInput("t2", "Número de tratamientos", value = 4, min = 2),
                 numericInput("Delta2", "Δ (diferencia mínima)", value = 3),
                 numericInput("sigma22", "σ² (varianza)", value = 10.35),
                 numericInput("alpha2", "Nivel α", value = 0.05, min = 0, max = 1, step = 0.01),
                 numericInput("power_target", "Potencia objetivo", value = 0.80, min = 0, max = 1, step = 0.01),
                 numericInput("r_min", "r mínimo", value = 2, min = 1),
                 numericInput("r_max", "r máximo", value = 100, min = 1),
                 actionButton("calc_rmin", "Calcular r mínimo")
        ),
        tabPanel("HHM",
                 numericInput("S1", "S1 (desv. estándar)", value = sqrt(141.6)),
                 numericInput("d_HHM", "d (Δ mínima)", value = 20),
                 numericInput("df2_HHM", "df2 (grados libertad)", value = 60),
                 numericInput("K_HHM", "K (tabla A.9)", value = 0.322),
                 actionButton("calc_HHM", "Calcular réplicas HHM")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "results",
        tabPanel("Potencia ANOVA", verbatimTextOutput("power_out")),
        tabPanel("r mínimo ANOVA", verbatimTextOutput("rmin_out")),
        tabPanel("HHM", verbatimTextOutput("HHM_out"))
      )
    )
  )
)

# Lógica del servidor
server <- function(input, output, session) {
  
  # Cálculo de potencia ANOVA
  observeEvent(input$calc_power, {
    res <- calcular_potencia(
      r     = input$r,
      t     = input$t,
      sigma2= input$sigma2,
      Delta = input$Delta,
      alpha = input$alpha
    )
    output$power_out <- renderPrint(res)
  })
  
  # Cálculo de r mínimo ANOVA
  observeEvent(input$calc_rmin, {
    res_r <- encontrar_r_minimo(
      t            = input$t2,
      sigma2       = input$sigma22,
      Delta        = input$Delta2,
      alpha        = input$alpha2,
      power_target = input$power_target,
      r_min        = input$r_min,
      r_max        = input$r_max
    )
    output$rmin_out <- renderPrint(paste("r mínimo:", res_r))
  })
  
  # Cálculo HHM
  observeEvent(input$calc_HHM, {
    r_hhm <- calcular_r_HHM(
      S1   = input$S1,
      d    = input$d_HHM,
      df2  = input$df2_HHM,
      K    = input$K_HHM
    )
    output$HHM_out <- renderPrint(r_hhm)
  })
  
}

# Ejecutar la app
shinyApp(ui = ui, server = server)
