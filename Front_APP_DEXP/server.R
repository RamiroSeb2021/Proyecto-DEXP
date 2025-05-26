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
  
  observeEvent(input$anterior_3.5, updateTabItems(session, "tabs", selected = "metodo_tukey"))
  observeEvent(input$anterior_3.5, updateTabItems(session, "tabs", selected = "metodo_tukey"))
  
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
  

  # Cálculo de Potencia  ------------------
  # 1) reactiveVal para almacenar el resultado
  resultado_pot <- reactiveVal(NULL)
  
  # 2) Define los outputs UNA sola vez, fuera del observeEvent:
  output$grafico_pot <- renderPlot({
    req(resultado_pot())
    resultado_pot()$grafico
  })
  output$tabla_pot <- DT::renderDT({
    req(resultado_pot())
    resultado_pot()$tabla
  }, options = list(
    pageLength      = 5,
    scrollY         = "300px",
    scrollCollapse  = TRUE,
    paging          = FALSE
  ))
  output$mensaje_pot <- renderText({
    req(resultado_pot())
    paste0(
      "Para alcanzar una potencia de ", input$beta_potencia,
      ", necesitas un tamaño muestral de ", resultado_pot()$r_optimo, "."
    )
  })
  
  # 3) El observeEvent con todo el control de errores:
  observeEvent(input$calcular_4, {
    
    # Cuando empiezas el cálculo:
    show("loading_pot_plot")
    hide("plot_pot_container")
    show("loading_pot_table")
    hide("table_pot_container")
    
    # 3.1) Deshabilita el botón mientras corre
    disable("calcular_4")
    on.exit(enable("calcular_4"), add = TRUE)
    
    # 3.2) Limpia resultado previo
    resultado_pot(NULL)
    
    # 1) t >= 2 y entero
    if (is.null(input$t_potencia) || is.na(input$t_potencia) || length(input$t_potencia) != 1 ||
        input$t_potencia < 2 || input$t_potencia != floor(input$t_potencia)) {
      show_error(Formato_tatamiento())
      return()
    }
    # 2) D > 0, numérico escalar
    if (is.null(input$Delta_potencia) || is.na(input$Delta_potencia) || length(input$Delta_potencia) != 1 ||
        !is.numeric(input$Delta_potencia) || input$Delta_potencia <= 0) {
      show_error(Formato_diferencia())
      return()
    }
    # 3) ro ≥ 1 entero
    if (is.null(input$pot_rho) || is.na(input$pot_rho) || length(input$pot_rho) != 1 ||
        input$pot_rho < 1 || input$pot_rho != floor(input$pot_rho)) {
      show_error(Formato_rho())
      return()
    }
    # 4) S1 > 0
    if (is.null(input$sigma2_potencia) || is.na(input$sigma2_potencia) || length(input$sigma2_potencia) != 1 ||
        input$sigma2_potencia <= 0) {
      show_error(Formato_sigma())
      return()
    }
    # 6) alfa ∈ (0,1)
    if (is.null(input$alpha_potencia) || is.na(input$alpha_potencia) || length(input$alpha_potencia) != 1 ||
        input$alpha_potencia <= 0 || input$alpha_potencia >= 1) {
      show_error(Formato_significancia())
      return()
    }
    # 7) Beta ∈ (0,1)
    if (is.null(input$beta_potencia) || is.na(input$beta_potencia) || length(input$beta_potencia) != 1 ||
        input$beta_potencia < 0 || input$beta_potencia >= 1) {
      show_error(Formato_Potencia())
      return()
    }
    
    # 3.4) Invocar la función con tryCatch()
    res_pot <- tryCatch(
      encontrar_r_minimo_Potencia(
        t   = input$t_potencia,
        rho   = input$pot_rho,
        dif    = input$Delta_potencia,
        potencia_objetivo = input$beta_potencia,
        sigma2_   = input$sigma2_potencia,
        alpha = input$alpha_potencia,
        
      ),
      error = function(e) {
        show_error(paste0("Error en simulación: ", e$message))
        NULL
      }
    )
    if (is.null(res_pot)) return()         # si hubo error, no continúa
    
    # 3.5) Todo OK: almacena el resultado y dispara los render
    resultado_pot(res_pot)
    
    
    hide("loading_pot_plot")
    show("plot_pot_container")
    hide("loading_pot_table")
    show("table_pot_container")
  })
  
  
  
  # Método Harris-Hurvitz-Mood con validaciones y mensajes de error ---------
  
  observeEvent(input$calcular_5, {
    
    # 1) Validaciones de entrada
    if (is.na(input$S2_1_hhm) || input$S2_1_hhm <= 0) {
      showModal(modalDialog(
        title = "Error",
        tags$p(style="white-space: normal; font-size: 15px;",
               "⚠️ S2₁ debe ser un número positivo mayor que cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."
        ),
        easyClose = TRUE, size = "s"
      ))
      return()
    }
    if (is.na(input$d_hhm) || input$d_hhm <= 0) {
      showModal(modalDialog(
        title = "Error",
        tags$p(style="white-space: normal; font-size: 15px;",
               "⚠️ La diferencia mínima detectable (d) debe ser mayor que cero. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."
        ),
        easyClose = TRUE, size = "s"
      ))
      return()
    }
    if (is.na(input$df2_hhm) || input$df2_hhm < 1 || input$df2_hhm %% 1 != 0) {
      showModal(modalDialog(
        title = "Error",
        tags$p(style="white-space: normal; font-size: 15px;",
               "⚠️ Los grados de libertad df₂ deben ser un entero mayor o igual a 1. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."
        ),
        easyClose = TRUE, size = "s"
      ))
      return()
    }
    if (is.na(input$alpha_hhm) || input$alpha_hhm <= 0 || input$alpha_hhm >= 1) {
      showModal(modalDialog(
        title = "Error",
        tags$p(style="white-space: normal; font-size: 15px;",
               "⚠️ El nivel de significancia (α) debe estar entre 0 y 1. Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información."
        ),
        easyClose = TRUE, size = "s"
      ))
      return()
    }
    
    # 2) Ejecutar cálculo
    resultado_hhm <- tryCatch(
      calcular_r_HHM(
        S2_1 = input$S2_1_hhm,
        d    = input$d_hhm,
        df2  = input$df2_hhm,
        alpha= input$alpha_hhm
      ),
      error = function(e) {
        showModal(modalDialog(
          title = "Error",
          tags$p(style="white-space: normal; font-size: 15px;",
                 paste0("⚠️ No se pudo calcular HHM: ", e$message,
                        " Por favor, revisa los datos ingresados y consulta el ícono ⓘ para más información.")
          ),
          easyClose = TRUE, size = "s"
        ))
        return(NULL)
      }
    )
    if (is.null(resultado_hhm)) return()
    
    # 3) Mostrar resultados
    output$resultados_5 <- renderText({
      paste0(
        "Resultados HHM:\n",
        "S₁ (desv. estándar redondeada): ", round(resultado_hhm$S1, 2), "\n",
        "K (tabla A.9): ", round(resultado_hhm$K, 3), "\n",
        "Número de réplicas (r): ", round(resultado_hhm$r, 2)
      )
    })
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
    
    # Cuando empiezas el cálculo:
    show("loading_sim_plot")
    hide("plot_sim_container")
    show("loading_sim_table")
    hide("table_sim_container")
    

    
    
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
    
   
    hide("loading_sim_plot")
    show("plot_sim_container")
    hide("loading_sim_table")
    show("table_sim_container")
  })
  

# Estimacion S1 y df1 -----------------------------------------------------

  observeEvent(input$calcular_s1_df1, {
    
    tryCatch({
      
      # a) Validar desviación estándar
      if (is.na(input$s1_est_sd) || length(input$s1_est_sd) != 1 || input$s1_est_sd <= 0) {
        show_error(Formato_S1())
        return()
      }
      
      # b) Validar Si
      if (is.na(input$s1_est_Si) || length(input$s1_est_Si) != 1 || input$s1_est_Si <= 0) {
        show_error(Formato_Si())
        return()
      }
      
      # c) Validar Ss
      if (is.na(input$s1_est_Ss) || length(input$s1_est_Ss) != 1 || input$s1_est_Ss <= 0) {
        show_error(Formato_Ss())
        return()
      }
      
      # d) Validar relación lógica Ss > Si
      if (input$s1_est_Ss <= input$s1_est_Si) {
        show_error(Formato_intervalo_relativo())
        return()
      }
      
      # e) Ejecutar función
      res <- calcular_S1_df1(
        desviacion_estandar = input$s1_est_sd,
        Si = input$s1_est_Si,
        Ss = input$s1_est_Ss
      )
      
      # f) Validación de resultado
      if (is.null(res)) {
        output$resultado_s1_df1 <- renderText({
          "⚠️ No se encontró un número de grados de libertad que cumpla el criterio."
        })
      } else {
        output$resultado_s1_df1 <- renderPrint({
          list(
            `S1 estimado` = round(res$S1, 3),
            `Grados de libertad (df1)` = res$grados_libertad,
            `Cociente crítico` = round(res$valor_x, 4),
            `Error relativo Grados de libertad` = paste0(round(res$error_relativo * 100, 2), " %")
          )
        })
      }
      
    }, error = function(e) {
      show_error(paste0("❌ Error inesperado: ", e$message))
      output$resultado_s1_df1 <- renderText({ "" })
    })
  })
  
  

}

