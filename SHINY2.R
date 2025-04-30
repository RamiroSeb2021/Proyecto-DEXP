library(shiny)
library(shinydashboard)

# Sourcing custom functions
source("Code/metodo_tukey.R")
source("Code/Costos.R")
source("Code/Potencia_Efectos_Aleatorios.R")
source("Code/Potencia.R")                   # calcular_potencia + encontrar_r_minimo
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
      # --- Introducción ---
      tabItem(tabName = "intro",
              fluidRow(
                box(title = "Bienvenido", width = 12, status = "primary", solidHeader = TRUE,
                    p("A continuación podrás realizar cálculos para determinar el número de réplicas en diferentes contextos de diseño experimental. Estos incluyen métodos con y sin costos, con efectos aleatorios, cálculos de potencia y el método de Harris-Hurvitz-Mood (HHM)."),
                    p("Haz clic en 'Continuar' para comenzar con el primer cálculo: Proporcionalidad sin Costo."),
                    actionButton("continuar", "Continuar", class = "btn btn-primary")
                )
              )
      ),
      
      # --- Proporcionalidad sin Costo ---
      tabItem(tabName = "sin_costo",
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("a", "Tratamientos (a)", 4),
                    numericInput("r0", "Réplicas iniciales (r₀)", 5),
                    textInput("sigmas", "Desviaciones σ (separadas por comas)", "6.27,9.57,12,3.32"),
                    actionButton("calcular_1", "Calcular", class = "btn btn-success")
                ),
                box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("resultados_1")
                )
              ),
              fluidRow(
                column(12, align = "right",
                       actionButton("siguiente_1", "Siguiente", icon = icon("arrow-right"), class = "btn btn-primary")
                )
              )
      ),
      
      # --- Proporcionalidad con Costo ---
      tabItem(tabName = "con_costo",
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("a_2", "Tratamientos (a)", 4),
                    textInput("sigmas_2", "Desviaciones σ", "6.27,9.57,12,3.32"),
                    textInput("costos", "Costos por tratamiento", "1000,200,700,1100"),
                    numericInput("costo_total", "Presupuesto total", 50000),
                    actionButton("calcular_2", "Calcular", class = "btn btn-success")
                ),
                box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("resultados_2")
                )
              ),
              fluidRow(
                column(6, align = "left",
                       actionButton("anterior_2", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                ),
                column(6, align = "right",
                       actionButton("siguiente_2", "Siguiente", icon = icon("arrow-right"), class = "btn btn-primary")
                )
              )
      ),
      
      # --- Efectos Aleatorios ---
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
                box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("resultados_3")
                )
              ),
              fluidRow(
                column(6, align = "left",
                       actionButton("anterior_3", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                ),
                column(6, align = "right",
                       actionButton("siguiente_3", "Siguiente", icon = icon("arrow-right"), class = "btn btn-primary")
                )
              )
      ),
      
      # --- Cálculo de Potencia ---
      tabItem(tabName = "potencia",
              fluidRow(
                box(title = "Parámetros", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("t_potencia", "Tratamientos (t)", 4, min = 1),
                    numericInput("sigma2_potencia", "Varianza estimada (σ²)", 10.35, min = 0),
                    numericInput("Delta_potencia", "Diferencia mínima detectable (Δ)", 3, min = -Inf),
                    numericInput("alpha_potencia", "Nivel de significancia (α)", 0.05, min = 0, max = 1, step = 0.01),
                    numericInput("power_target", "Potencia objetivo (1−β)", 0.80, min = 0, max = 1, step = 0.01),
                    actionButton("calcular_4", "Calcular", class = "btn btn-success")
                ),
                box(title = "Resultados", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("resultados_4")
                )
              ),
              fluidRow(
                column(6, align = "left",
                       actionButton("anterior_4", "Anterior", icon = icon("arrow-left"), class = "btn btn-secondary")
                ),
                column(6, align = "right",
                       actionButton("siguiente_4", "Siguiente", icon = icon("arrow-right"), class = "btn btn-primary")
                )
              )
      ),
      
      # --- Método HHM ---
      tabItem(tabName = "hhm",
              fluidRow(
                box(title = "Parámetros HHM", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("S2_1_hhm", "Varianza estimada S2₁ (g²)", 141.6, min = 0),
                    numericInput("d_hhm", "Diferencia mínima detectable d (gramos)", 20, min = 0),
                    numericInput("df2_hhm", "Grados de libertad df₂ (entero)", 60, min = 1),
                    numericInput("alpha_hhm", "Nivel de significancia α (por ejemplo, 0.05)", 0.05, min = 0, max = 1, step = 0.01),
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
  # Navegación
  observeEvent(input$continuar, updateTabItems(session, "tabs", selected = "sin_costo"))
  observeEvent(input$siguiente_1, updateTabItems(session, "tabs", selected = "con_costo"))
  observeEvent(input$anterior_2, updateTabItems(session, "tabs", selected = "sin_costo"))
  observeEvent(input$siguiente_2, updateTabItems(session, "tabs", selected = "efectos"))
  observeEvent(input$anterior_3, updateTabItems(session, "tabs", selected = "con_costo"))
  observeEvent(input$siguiente_3, updateTabItems(session, "tabs", selected = "potencia"))
  observeEvent(input$anterior_4, updateTabItems(session, "tabs", selected = "efectos"))
  observeEvent(input$siguiente_4, updateTabItems(session, "tabs", selected = "hhm"))
  
  # Proporcionalidad sin Costo
  observeEvent(input$calcular_1, {
    sigmas <- as.numeric(unlist(strsplit(input$sigmas, ",")))
    out1 <- proporcionalidad_sin_costo_ni_tamaño_de_muestra(input$a, input$r0, sigmas)
    output$resultados_1 <- renderText(paste("Réplicas asignadas:", paste(out1, collapse = ", ")))
  })
  
  # Proporcionalidad con Costo
  observeEvent(input$calcular_2, {
    sigmas <- as.numeric(unlist(strsplit(input$sigmas_2, ",")))
    costos <- as.numeric(unlist(strsplit(input$costos, ",")))
    out2 <- proporcionalidad_con_costo_ni_tamaño_de_muestra(input$a_2, sigmas, costos, input$costo_total)
    output$resultados_2 <- renderText(paste("Réplicas asignadas:", paste(out2, collapse = ", ")))
  })
  
  # Efectos Aleatorios
  observeEvent(input$calcular_3, {
    res3 <- numero_de_tratamientos_y_replicas_con_efectos_aleatorios(
      input$costo_tratamiento, input$costo_ue, input$sigma_cuadrado, input$rho, input$v_max
    )
    output$resultados_3 <- renderText(
      paste0("Tratamientos: ", res3$num_de_tratamientos,
             "\nRéplicas por tratamiento: ", res3$num_de_replicas)
    )
  })
  
  # Cálculo de Potencia (con validaciones)
  observeEvent(input$calcular_4, {
    # Validaciones
    if (is.na(input$t_potencia) || input$t_potencia %% 1 != 0 || input$t_potencia < 2) {
      showNotification("Error: t debe ser un entero ≥ 2.", type = "error"); return()
    }
    if (is.na(input$sigma2_potencia) || input$sigma2_potencia <= 0) {
      showNotification("Error: σ² debe ser positivo.", type = "error"); return()
    }
    if (is.na(input$Delta_potencia) || input$Delta_potencia <= 0) {
      showNotification("Error: Δ debe ser > 0.", type = "error"); return()
    }
    if (is.na(input$alpha_potencia) || input$alpha_potencia <= 0 || input$alpha_potencia >= 1) {
      showNotification("Error: α debe estar en (0,1).", type = "error"); return()
    }
    if (is.na(input$power_target) || input$power_target <= 0 || input$power_target >= 1) {
      showNotification("Error: potencia objetivo debe estar en (0,1).", type = "error"); return()
    }
    
    res4 <- tryCatch(
      encontrar_r_minimo(
        t = input$t_potencia,
        sigma2 = input$sigma2_potencia,
        Delta = input$Delta_potencia,
        alpha = input$alpha_potencia,
        power_target = input$power_target
      ),
      error = function(e) {
        showNotification(paste("No se pudo calcular:", e$message), type = "error")
        return(NULL)
      }
    )
    if (is.null(res4)) return()
    
    output$resultados_4 <- renderPrint({
      cat(sprintf("Número mínimo de réplicas (r): %d\n", res4$r))
      cat(sprintf("φ² (no centralidad): %.4f\n", res4$phi2))
      cat(sprintf("df₁ (t−1): %d\n", res4$df1))
      cat(sprintf("df₂ (t·(r−1)): %d\n", res4$df2))
      cat(sprintf("F crítico (1−α): %.4f\n", res4$Fcrit))
      cat(sprintf("Potencia alcanzada (1−β): %.4f\n", res4$power))
    })
  })
  
  # Método HHM (corregido)
  observeEvent(input$calcular_5, {
    # Validaciones HHM
    if (is.na(input$S2_1_hhm) || input$S2_1_hhm <= 0) {
      showNotification("Error: S2₁ debe ser > 0.", type = "error"); return()
    }
    if (is.na(input$d_hhm) || input$d_hhm <= 0) {
      showNotification("Error: d debe ser > 0.", type = "error"); return()
    }
    if (is.na(input$df2_hhm) || input$df2_hhm < 1) {
      showNotification("Error: df₂ debe ser ≥ 1.", type = "error"); return()
    }
    if (is.na(input$alpha_hhm) || input$alpha_hhm <= 0 || input$alpha_hhm >= 1) {
      showNotification("Error: α debe estar en (0,1).", type = "error"); return()
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

