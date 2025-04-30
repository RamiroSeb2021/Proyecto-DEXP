# control_bienvenida.R
# Aquí definimos la función que muestra la modal de bienvenida

show_bienvenida_modal <- function() {
  showModal(
    modalDialog(
      size      = "l",
      easyClose = FALSE,
      fade      = FALSE,
      # Contenedor con fondo GIF
      HTML("
        <div style=\"
          background: url('analizar.gif') center center / cover no-repeat;
          width:100%; height:80vh;
          display:flex; align-items:center; justify-content:center;
        \">
          <div style=\"
            background: rgba(255,255,255,0.9);
            padding: 40px; border-radius: 15px;
            max-width:600px; text-align:center;
            box-shadow:0 0 30px rgba(0,0,0,0.3);
          \">
            <img src='1361b70b-cd7c-4150-aa30-e3f659f15fba.png' style='width:120px;margin-bottom:20px;'/>
            <h1>Bienvenido a la App de Diseño de Experimentos</h1>
            <p>Explora datos, modelos experimentales y calcula tamaños muestrales.</p>
            <p>Estudiantes: Juan Sebastián Ramirez Ayala, Diana Catalina Hernández Rojas, Yan Carlos Moreno Guerra</p>
            <p>Profesor: Wilmer Dario Pineda Rios</p>
          </div>
        </div>
      "),
      footer = actionButton("ingresar", "Ingresar", class = "btn btn-success btn-lg")
    )
  )
}


# control_bienvenida <- function(input, output, session, started, dashboard_ui) {
#   
#   output$main_ui <- renderUI({
#     if (!started()) {
#       # Estilos solo para la bienvenida
#       fluidPage(
#         tags$head(
#           tags$style(HTML("
#             body {
#               background-image: url('data-analytics.gif');
#               background-size: cover;
#               background-position: center;
#               background-repeat: no-repeat;
#               background-attachment: fixed;
#               font-family: 'Helvetica Neue', sans-serif;
#             }
# 
#             .welcome-box {
#               background-color: rgba(255, 255, 255, 0.9);
#               border-radius: 15px;
#               padding: 40px;
#               max-width: 650px;
#               margin: 100px auto;
#               text-align: center;
#               box-shadow: 0 0 30px rgba(0,0,0,0.4);
#             }
# 
#             .welcome-logo {
#               width: 120px;
#               margin-bottom: 20px;
#             }
#           "))
#         ),
#         div(
#           class = "welcome-box",
#           img(src = "1361b70b-cd7c-4150-aa30-e3f659f15fba.png", class = "welcome-logo"),
#           h1("Bienvenido a la App de Diseño de Experimentos"),
#           p("Explora análisis de datos, modelos experimentales y cálculos de tamaño de muestra."),
#           br(),
#           actionBttn("start", "Ingresar", style = "jelly", color = "success", size = "lg")
#         )
#       )
#     } else {
#       tagList(
#         tags$head(
#           tags$style(HTML("
#         body, .content-wrapper, .right-side, .skin-blue, .wrapper {
#           background-image: none !important;
#           background-color: #ecf0f5 !important;
#           background: #ecf0f5 !important;
#         }
#       "))
#         ),
#         dashboard_ui
#       )
#     }
#   })
#   
#   observeEvent(input$start, {
#     started(TRUE)
#   })
# }
# 
