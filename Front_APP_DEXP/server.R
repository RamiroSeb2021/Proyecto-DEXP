#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



# Cargar funciones
source("Code/metodo_tukey.R")
source("Code/Costos.R")
source("Code/Potencia_Efectos_Aleatorios.R")
source("Code/Potencia.R")
source("Code/metodo_Harris_Hurvitz_Mood.R")
source("Code/control_bienvenida.R")
source("Code/errores.R")

server <- function(input, output, session) {
  # 1) Abrimos la splash modal al iniciar
  show_bienvenida_modal()
  
  # 2) Cuando el usuario hace clic en "Ingresar", cerramos la modal
  observeEvent(input$ingresar, {
    removeModal()
  })
  
  # 3) Navegación interna del dashboard
  observeEvent(input$continuar, {
    updateTabItems(session, "tabs", selected = "sin_costo")
  })
  
  # Navegación
  observeEvent(input$siguiente_1, updateTabItems(session, "tabs", selected = "con_costo"))
  observeEvent(input$anterior_2, updateTabItems(session, "tabs", selected = "sin_costo"))
  observeEvent(input$siguiente_2, updateTabItems(session, "tabs", selected = "efectos"))
  observeEvent(input$anterior_3, updateTabItems(session, "tabs", selected = "con_costo"))
  observeEvent(input$siguiente_3, updateTabItems(session, "tabs", selected = "potencia"))
  observeEvent(input$anterior_4, updateTabItems(session, "tabs", selected = "efectos"))
  observeEvent(input$siguiente_4, updateTabItems(session, "tabs", selected = "hhm"))
  observeEvent(input$anterior_5, updateTabItems(session, "tabs", selected = "potencia"))
  
  # Función para mostrar errores en modal y limpiar output
  show_error <- function(message) {
    showModal(modalDialog(
      title = "Error",
      paste0("⚠️ ", message),
      easyClose = TRUE,
      size = "s"
    ))
    output$resultados_1 <- renderText({ NULL })  # Limpia resultados
  }
  
  observeEvent(input$calcular_1, {
    # Limpia resultados previos
    output$resultados_1 <- renderText({ NULL })
    
    # Validar y obtener sigmas con función externa que llama a show_error si hay problema
    sigmas <- Excepciones_proporcionalidad_sin_costo(input$a, input$r0, input$sigmas, show_error)
    if (isFALSE(sigmas) || is.null(sigmas)) return()  # Si hay error, salir
    
    # Ejecutar cálculo con manejo de errores
    resultados <- tryCatch(
      proporcionalidad_sin_costo_ni_tamaño_de_muestra(a = input$a, r0 = input$r0, sigmas = sigmas),
      error = function(e) {
        show_error(paste("Error en el cálculo:", e$message))
        NULL
      }
    )
    if (is.null(resultados)) return()
    
    # Mostrar resultados
    output$resultados_1 <- renderText({
      paste("Réplicas asignadas:", paste(resultados, collapse = ", "))
    })
  })
  
  
  # Cálculos
  #observeEvent(input$calcular_1, {
    #sigmas <- as.numeric(unlist(strsplit(input$sigmas, ",")))
    #resultados <- proporcionalidad_sin_costo_ni_tamaño_de_muestra(input$a, input$r0, sigmas)
    #output$resultados_1 <- renderText({ paste("Réplicas asignadas:", paste(resultados, collapse = ", ")) })
  #})
  
  observeEvent(input$calcular_2, {
    sigmas <- as.numeric(unlist(strsplit(input$sigmas_2, ",")))
    costos <- as.numeric(unlist(strsplit(input$costos, ",")))
    resultados <- proporcionalidad_con_costo_ni_tamaño_de_muestra(input$a_2, sigmas, costos, input$costo_total)
    output$resultados_2 <- renderText({ paste("Réplicas asignadas:", paste(resultados, collapse = ", ")) })
  })
  
  observeEvent(input$calcular_3, {
    resultados <- numero_de_tratamientos_y_replicas_con_efectos_aleatorios(
      input$costo_tratamiento, input$costo_ue, input$sigma_cuadrado, input$rho, input$v_max)
    output$resultados_3 <- renderText({ paste("Tratamientos:", resultados$num_de_tratamientos,
                                              "\nRéplicas por tratamiento:", resultados$num_de_replicas) })
  })
  
  observeEvent(input$calcular_4, {
    resultados <- calcular_potencia(
      input$r_potencia, input$t_potencia, input$sigma2_potencia, input$Delta_potencia, input$alpha_potencia)
    output$resultados_4 <- renderPrint({ resultados })
  })
  
  observeEvent(input$calcular_5, {
    resultado <- calcular_r_HHM(input$S1_hhm, input$d_hhm, input$df2_hhm, input$K_hhm)
    output$resultados_5 <- renderText({ paste("Número estimado de réplicas (r):", round(resultado, 2)) })
  })
  
  observeEvent(input$calcular_mt, {
    res <- calcular_r_MT(
      T_    = input$mt_T,
      D     = input$mt_D,
      ro    = input$mt_ro,
      S1    = input$mt_S1,
      df1   = input$mt_df1,
      alfa  = input$mt_alfa,
      Beta  = input$mt_Beta
    )
    output$resultados_mt <- renderPrint(res)
  })
  
  # 2) Simulación de potencia / r mínimo
  observeEvent(input$calcular_sim, {
    resultado <- encontrar_r_minimo(
      t            = input$sim_t,
      sigma2       = input$sim_sigma2,
      Delta        = input$sim_Delta,
      alpha        = input$sim_alpha,
      power_target = input$sim_power_target,
      r_min        = input$sim_r_min,
      r_max        = input$sim_r_max
    )
    output$resultados_sim <- renderPrint({
      if (is.null(resultado)) "No se alcanzó la potencia deseada" else
        list(r_optimo = resultado$r_optimo,
             potencia  = resultado$potencia)
    })
    output$grafico_sim <- renderPlot({
      if (!is.null(resultado)) resultado$grafico
    })
    output$tabla_sim <- renderDataTable({
      if (!is.null(resultado)) resultado$tabla
    }, options = list(pageLength = 5))
  })
}
