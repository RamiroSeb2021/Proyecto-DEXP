library(shiny)
library(shinydashboard)

# Sourcing custom functions
source("Code/metodo_tukey.R")
source("Code/Costos.R")
source("Code/Potencia_Efectos_Aleatorios.R")
source("Code/Potencia.R")                   # contiene calcular_r_teorica
source("Code/metodo_Harris_Hurvitz_Mood.R")  # obtener_K_A9 + calcular_r_HHM

ui <- dashboardPage(
  dashboardHeader(title = "Diseño Experimental"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Introducción", tabName = "intro", icon = icon("info-circle")),
                menuItem("Cálculos número de réplicas", icon = icon("flask"),
                         menuSubItem("Proporcionalidad sin Costo", tabName = "sin_costo"),
                         menuSubItem("Proporcionalidad con Costo", tabName = "con_costo"),
                         menuSubItem("Efectos Aleatorios", tabName = "efectos"),
                         menuSubItem("Cálculo de Potencia", tabName = "potencia"),
                         menuSubItem("Método HHM", tabName = "hhm")
                )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(title = "Bienvenido", width = 12, status = "primary", solidHeader = TRUE,
                    p("A continuación podrás realizar cálculos para determinar el número de réplicas en diferentes contextos de diseño experimental. Estos incluyen métodos con y sin costos, con efectos aleatorios, cálculos de potencia y el método de Harris-Hurvitz-Mood (HHM)."),
                    p("Haz clic en 'Continuar' para comenzar con el primer cálculo: Proporcionalidad sin Costo."),
                    actionButton("continuar", "Continuar", class = "btn btn-primary")
                )
              )
      ),
      
      # Potencia (modificado)
      tabItem(tabName = "potencia",
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("t_potencia", "Tratamientos (t)", 4, min = 2),
                    numericInput("sigma2_potencia", "Varianza estimada (\u03c3\u00b2)", 10.35, min = 0.001),
                    numericInput("Delta_potencia", "Diferencia mínima detectable (\u0394)", 3, min = 0.001),
                    numericInput("alpha_potencia", "Nivel de significancia (\u03b1)", 0.05, min = 0, max = 1, step = 0.01),
                    numericInput("power_target", "Potencia objetivo (1−\u03b2)", 0.80, min = 0, max = 1, step = 0.01),
                    actionButton("calcular_4", "Calcular", class = "btn btn-success")
                ),
                box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("resultados_4")
                )
              )
      ),
      
      # Método HHM (sin cambios)
      tabItem(tabName = "hhm",
              fluidRow(
                box(title = "Parámetros HHM", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("S2_1_hhm", "Varianza estimada S2\u2081 (g\u00b2)", 141.6, min = 0),
                    numericInput("d_hhm", "Diferencia mínima detectable d (gramos)", 20, min = 0),
                    numericInput("df2_hhm", "Grados de libertad df\u2082 (entero)", 60, min = 1),
                    numericInput("alpha_hhm", "Nivel de significancia \u03b1 (por ejemplo, 0.05)", 0.05, min = 0, max = 1, step = 0.01),
                    actionButton("calcular_5", "Calcular", class = "btn btn-success")
                ),
                box(title = "Resultados HHM", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("resultados_5")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$continuar, updateTabItems(session, "tabs", selected = "sin_costo"))
  
  observeEvent(input$calcular_4, {
    if (is.na(input$t_potencia) || input$t_potencia %% 1 != 0 || input$t_potencia < 2) {
      showNotification("Error: t debe ser un entero ≥ 2.", type = "error"); return()
    }
    if (is.na(input$sigma2_potencia) || input$sigma2_potencia <= 0) {
      showNotification("Error: \u03c3\u00b2 debe ser positivo.", type = "error"); return()
    }
    if (is.na(input$Delta_potencia) || input$Delta_potencia <= 0) {
      showNotification("Error: \u0394 debe ser > 0.", type = "error"); return()
    }
    if (is.na(input$alpha_potencia) || input$alpha_potencia <= 0 || input$alpha_potencia >= 1) {
      showNotification("Error: \u03b1 debe estar en (0,1).", type = "error"); return()
    }
    if (!(input$power_target %in% c(0.80, 0.82, 0.90))) {
      showNotification("Error: Potencia debe ser 0.80, 0.82 o 0.90 (según tablas).", type = "error"); return()
    }
    
    res4 <- tryCatch(
      calcular_r_teorica(
        t = input$t_potencia,
        sigma2 = input$sigma2_potencia,
        Delta = input$Delta_potencia,
        power_target = input$power_target,
        alpha = input$alpha_potencia
      ),
      error = function(e) {
        showNotification(paste("No se pudo calcular:", e$message), type = "error")
        return(NULL)
      }
    )
    if (is.null(res4)) return()
    
    output$resultados_4 <- renderPrint({
      cat("\U0001F4DD Resultado según teoría (libro):\n")
      cat(sprintf("Número de réplicas (r): %d\n", res4$r))
      cat(sprintf("\u03c6: %.3f\n", res4$phi))
      cat(sprintf("\u03c6²: %.3f\n", res4$phi2))
      cat(sprintf("df₁ (t−1): %d\n", res4$df1))
      cat(sprintf("df₂ (t\u22c5(r−1)): %d\n", res4$df2))
      cat(sprintf("Potencia objetivo (1−\u03b2): %.2f\n", res4$potencia_objetivo))
    })
  })
  
  observeEvent(input$calcular_5, {
    if (is.na(input$S2_1_hhm) || input$S2_1_hhm <= 0) {
      showNotification("Error: S2\u2081 debe ser > 0.", type = "error"); return()
    }
    if (is.na(input$d_hhm) || input$d_hhm <= 0) {
      showNotification("Error: d debe ser > 0.", type = "error"); return()
    }
    if (is.na(input$df2_hhm) || input$df2_hhm < 1) {
      showNotification("Error: df\u2082 debe ser ≥ 1.", type = "error"); return()
    }
    if (is.na(input$alpha_hhm) || input$alpha_hhm <= 0 || input$alpha_hhm >= 1) {
      showNotification("Error: \u03b1 debe estar en (0,1).", type = "error"); return()
    }
    
    res5 <- calcular_r_HHM(
      S2_1 = input$S2_1_hhm,
      d    = input$d_hhm,
      df2  = input$df2_hhm,
      alpha= input$alpha_hhm
    )
    
    output$resultados_5 <- renderPrint({
      cat(sprintf("S₁ (desv. estándar redondeada): %.2f\n", res5$S1))
      cat(sprintf("K (tabla A.9): %.3f\n", res5$K))
      cat(sprintf("Número de réplicas (r): %.2f\n", res5$r))
    })
  })
}

shinyApp(ui = ui, server = server)
