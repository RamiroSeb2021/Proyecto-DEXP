ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML("
      .tab-content { padding: 20px; }
      .well { background-color: #f5f5f5; border: 1px solid #ddd; }
      .input-group { margin-bottom: 15px; }
      .btn-primary { background: #007bff; }
      .result-box { 
        background: #f8f9fa; 
        padding: 20px; 
        border-radius: 8px; 
        border: 1px solid #dee2e6;
      }
    "))
  ),
  
  titlePanel(
    div(
      h1("Diseño Experimental Optimizado", style = "color: #004080;"),
      img(src = "https://i.imgur.com/3JZ7X6F.png", height = 60, style = "float:right;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
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
          )
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resultados", 
                 div(
                   class = "result-box",
                   h4("Resultados del cálculo"),
                   verbatimTextOutput("resultados")
                 )
        )
      )
    )
  )
)
