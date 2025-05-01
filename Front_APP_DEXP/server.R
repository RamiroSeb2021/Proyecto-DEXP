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
  observeEvent(input$siguiente_5, updateTabItems(session, "tabs", selected = "metodo_tukey"))
  observeEvent(input$anterior_6, updateTabItems(session, "tabs", selected = "hhm"))
  observeEvent(input$siguiente_6, updateTabItems(session, "tabs", selected = "sim_potencia"))
  observeEvent(input$anterior_7, updateTabItems(session, "tabs", selected = "metodo_tukey"))
  
  # Función para mostrar errores en modal y limpiar output
  show_error <- function(message) {
    showModal(modalDialog(
      title = "Error",
      tags$p(
        style = "white-space: normal; font-size: 15px; line-height: 1.5;",
        paste0("⚠️ ", message)
      ),
      easyClose = TRUE,
      size = "s"  # cambia de "s" a "m" o "l" para permitir más espacio
    ))
    output$resultados_1 <- renderText({ NULL })
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
  
  observeEvent(input$calcular_2, {
    # Limpiar resultados previos
    output$resultados_2 <- renderText({ NULL })
    
    # Validar entradas y obtener sigmas y costos
    validados <- Excepciones_proporcionalidad_con_costo(
      a = input$a_2,
      sigmas_str = input$sigmas_2,
      costos_str = input$costos,
      costo_total = input$costo_total,
      show_error = show_error
    )
    
    # Si hubo errores en la validación de entradas, salir
    if (isFALSE(validados)) return()
    
    # Validar que los costos no sean negativos o cero antes de proceder
    if (any(validados$costos <= 0)) {
      show_error("Los costos no pueden ser negativos ni cero.")
      return()  # Detener la ejecución si hay costos inválidos
    }
    
    # Verificar que los sigmas no sean vacíos o no válidos
    if (is.null(validados$sigmas) || length(validados$sigmas) == 0) {
      show_error("Las desviaciones estándar no están definidas correctamente.")
      return()  # Detener la ejecución si los sigmas son inválidos
    }
    
    # Verificar que los costos sean válidos
    if (is.null(validados$costos) || length(validados$costos) == 0) {
      show_error("Los costos no están definidos correctamente.")
      return()  # Detener la ejecución si los costos son inválidos
    }
    
    # Ejecutar cálculo con manejo de errores
    resultados <- tryCatch(
      proporcionalidad_con_costo_ni_tamaño_de_muestra(
        a = input$a_2,
        sigmas = validados$sigmas,
        costos = validados$costos,
        costo_total = input$costo_total
      ),
      error = function(e) {
        show_error(paste("Error en el cálculo:", e$message))
        return(NULL)  # Devuelve NULL si ocurre un error
      }
    )
    
    # Si hubo error en el cálculo, salir
    if (is.null(resultados)) return()
    
    # Mostrar resultados
    output$resultados_2 <- renderText({
      paste("Réplicas asignadas:", paste(resultados, collapse = ", "))
    })
  })
  
  # Observador para calcular los resultados
  observeEvent(input$calcular_3, {
    
    # Validando los parámetros de entrada
    validado <- validar_parametros_funcion_disenio(
      input$costo_tratamiento, input$costo_ue, input$sigma_cuadrado, input$rho, input$v_max, show_error
    )
    
    # Si la validación falla, no continuamos con el cálculo
    if (!validado) {
      return(NULL)
    }
    
    # Si la validación es exitosa, calculamos los resultados
    resultados <- numero_de_tratamientos_y_replicas_con_efectos_aleatorios(
      input$costo_tratamiento, input$costo_ue, input$sigma_cuadrado, input$rho, input$v_max
    )
    
    # Actualizando el output con los resultados
    output$resultados_3 <- renderText({
      paste("Tratamientos:", resultados$num_de_tratamientos,
            "\nRéplicas por tratamiento:", resultados$num_de_replicas)
    })
  })
  
  
  
  
  
  
  
  
  
  
  # Cálculos
  #observeEvent(input$calcular_1, {
    #sigmas <- as.numeric(unlist(strsplit(input$sigmas, ",")))
    #resultados <- proporcionalidad_sin_costo_ni_tamaño_de_muestra(input$a, input$r0, sigmas)
    #output$resultados_1 <- renderText({ paste("Réplicas asignadas:", paste(resultados, collapse = ", ")) })
  #})
  
  #observeEvent(input$calcular_2, {
    #sigmas <- as.numeric(unlist(strsplit(input$sigmas_2, ",")))
    #costos <- as.numeric(unlist(strsplit(input$costos, ",")))
    #resultados <- proporcionalidad_con_costo_ni_tamaño_de_muestra(input$a_2, sigmas, costos, input$costo_total)
    #output$resultados_2 <- renderText({ paste("Réplicas asignadas:", paste(resultados, collapse = ", ")) })
  #})
  
  #observeEvent(input$calcular_3, {
    #resultados <- numero_de_tratamientos_y_replicas_con_efectos_aleatorios(
      #input$costo_tratamiento, input$costo_ue, input$sigma_cuadrado, input$rho, input$v_max)
    #output$resultados_3 <- renderText({ paste("Tratamientos:", resultados$num_de_tratamientos,
                                              #"\nRéplicas por tratamiento:", resultados$num_de_replicas) })
  #})
  
  observeEvent(input$calcular_4, {
    resultados <- calcular_potencia(
      input$r_potencia, input$t_potencia, input$sigma2_potencia, input$Delta_potencia, input$alpha_potencia)
    output$resultados_4 <- renderPrint({ resultados })
  })
  
  observeEvent(input$calcular_5, {
    resultado <- calcular_r_HHM(input$S1_hhm, input$d_hhm, input$df2_hhm, input$K_hhm)
    output$resultados_5 <- renderText({ paste("Número estimado de réplicas (r):", round(resultado, 2)) })
  })
  

# Calcular metodo de tukey ------------------------------------------------
  # reactiveVal para almacenar el resultado
  resultado_mt <- reactiveVal(NULL)
  
  # define el output fuera del observeEvent, con req()
  output$resultados_mt <- renderPrint({
    req(resultado_mt())      # si es NULL, no dibuja nada
    resultado_mt()
  })
  
  observeEvent(input$calcular_mt, {
    # Limpiar cualquier modal / output previo
    resultado_mt(NULL)

    # 1) t >= 2 y entero
    if (is.null(input$mt_T) || is.na(input$mt_T) || length(input$mt_T) != 1 ||
        input$mt_T < 2 || input$mt_T != floor(input$mt_T)) {
      show_error(Formato_tatamiento())
      return()
    }
    # 2) D > 0, numérico escalar
    if (is.null(input$mt_D) || is.na(input$mt_D) || length(input$mt_D) != 1 ||
        !is.numeric(input$mt_D) || input$mt_D <= 0) {
      show_error(Formato_diferencia())
      return()
    }
    # 3) ro ≥ 1 entero
    if (is.null(input$mt_ro) || is.na(input$mt_ro) || length(input$mt_ro) != 1 ||
        input$mt_ro < 1 || input$mt_ro != floor(input$mt_ro)) {
      show_error(Formato_rho())
      return()
    }
    # 4) S1 > 0
    if (is.null(input$mt_S1) || is.na(input$mt_S1) || length(input$mt_S1) != 1 ||
        input$mt_S1 <= 0) {
      show_error(Formato_sigma())
      return()
    }
    # 5) df1 ≥ 1 entero
    if (is.null(input$mt_df1) || is.na(input$mt_df1) || length(input$mt_df1) != 1 ||
        input$mt_df1 < 1 || input$mt_df1 != floor(input$mt_df1)) {
      show_error(Formato_gradosLibertad())
      return()
    }
    # 6) alfa ∈ (0,1)
    if (is.null(input$mt_alfa) || is.na(input$mt_alfa) || length(input$mt_alfa) != 1 ||
        input$mt_alfa <= 0 || input$mt_alfa >= 1) {
      show_error(Formato_significancia())
      return()
    }
    # 7) Beta ∈ (0,1)
    if (is.null(input$mt_Beta) || is.na(input$mt_Beta) || length(input$mt_Beta) != 1 ||
        input$mt_Beta < 0 || input$mt_Beta >= 1) {
      show_error(Formato_Potencia())
      return()
    }
    
    # Si todo pasó, ejecutamos y capturamos errores de la función
    resultados <- tryCatch(
      calcular_r_MT(
        T_   = input$mt_T,
        D    = input$mt_D,
        ro   = input$mt_ro,
        S1   = input$mt_S1,
        df1  = input$mt_df1,
        alfa = input$mt_alfa,
        Beta = input$mt_Beta
      ),
      error = function(e) {
        show_error(paste("Error en Tukey:", e$message))
        return(NULL)
      }
    )
    if (is.null(resultados)) return()
    
    resultado_mt(resultados)
  })

# Simulación de potencia / r mínimo ---------------------------------------

  # 1) reactiveVal para almacenar el resultado
  resultado_sim <- reactiveVal(NULL)
  
  # 2) Define los outputs UNA sola vez, fuera del observeEvent:
  output$grafico_sim <- renderPlot({
    req(resultado_sim())
    resultado_sim()$grafico
  })
  output$tabla_sim <- DT::renderDT({
    req(resultado_sim())
    resultado_sim()$tabla
  }, options = list(
    pageLength      = 5,
    scrollY         = "300px",
    scrollCollapse  = TRUE,
    paging          = FALSE
  ))
  output$mensaje_sim <- renderText({
    req(resultado_sim())
    paste0(
      "Para alcanzar una potencia de ", input$sim_power_target,
      ", necesitas un tamaño muestral de ", resultado_sim()$r_optimo, "."
    )
  })
  
  # 3) El observeEvent con todo el control de errores:
  observeEvent(input$calcular_sim, {
    # 3.1) Deshabilita el botón mientras corre
    disable("calcular_sim")
    on.exit(enable("calcular_sim"), add = TRUE)
    
    # 3.2) Limpia resultado previo
    resultado_sim(NULL)
    
    # 3.3) Validaciones de formato y rango
    #   a) t ≥ 2 entero
    if (is.na(input$sim_t) || length(input$sim_t) != 1 ||
        input$sim_t < 2 || input$sim_t != floor(input$sim_t)) {
      show_error(Formato_tatamiento())
      return()
    }
    #   b) rho ≥ 0 escalar
    if (is.na(input$sim_rho) || length(input$sim_rho) != 1 ||
        input$sim_rho < 0) {
      show_error(Formato_rho())
      return()
    }
    #   c) sigma2 > 0 escalar
    if (is.na(input$sim_sigma2) || length(input$sim_sigma2) != 1 ||
        input$sim_sigma2 <= 0) {
      show_error(Formato_sigma())
      return()
    }
    #   d) alfa ∈ (0,1)
    if (is.na(input$sim_alpha) || length(input$sim_alpha) != 1 ||
        input$sim_alpha <= 0 || input$sim_alpha >= 1) {
      show_error(Formato_significancia())
      return()
    }
    #   e) potencia ∈ (0,1)
    if (is.na(input$sim_power_target) || length(input$sim_power_target) != 1 ||
        input$sim_power_target <= 0 || input$sim_power_target >= 1) {
      show_error(Formato_Potencia())
      return()
    }
    #   f) r_max ≥ 1 entero
    if (is.na(input$sim_r_max) || length(input$sim_r_max) != 1 ||
        input$sim_r_max < 1 || input$sim_r_max != floor(input$sim_r_max)) {
      show_error(Formato_r_max())
      return()
    }
    
    # 3.4) Invocar la función con tryCatch()
    res <- tryCatch(
      encontrar_r_minimo_(
        t                  = input$sim_t,
        rho                = input$sim_rho,
        potencia_objetivo  = input$sim_power_target,
        sigma2             = input$sim_sigma2,
        alpha              = input$sim_alpha,
        r_max              = input$sim_r_max
      ),
      error = function(e) {
        show_error(paste0("Error en simulación: ", e$message))
        NULL
      }
    )
    if (is.null(res)) return()         # si hubo error, no continúa
    
    # 3.5) Todo OK: almacena el resultado y dispara los render
    resultado_sim(res)
  })

}

