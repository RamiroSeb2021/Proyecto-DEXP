server <- function(input, output, session) {
  
  # Calculo 1
  observeEvent(input$calcular_1, {
    sigmas <- as.numeric(unlist(strsplit(input$sigmas, ",")))
    resultados <- proporcionalidad_sin_costo_ni_tamaño_de_muestra(
      a = input$a, 
      r0 = input$r0, 
      sigmas = sigmas
    )
    output$resultados <- renderText({
      paste("Réplicas asignadas:", paste(resultados, collapse = ", "))
    })
  })
  
  # Calculo 2
  observeEvent(input$calcular_2, {
    sigmas <- as.numeric(unlist(strsplit(input$sigmas_2, ",")))
    costos <- as.numeric(unlist(strsplit(input$costos, ",")))
    resultados <- proporcionalidad_con_costo_ni_tamaño_de_muestra(
      a = input$a_2, 
      sigmas = sigmas, 
      costos = costos, 
      costo_total = input$costo_total
    )
    output$resultados <- renderText({
      paste("Réplicas asignadas:", paste(resultados, collapse = ", "))
    })
  })
  
  # Calculo 3
  observeEvent(input$calcular_3, {
    resultados <- numero_de_tratamientos_y_replicas_con_efectos_aleatorios(
      costo_tratamiento = input$costo_tratamiento, 
      costo_ue = input$costo_ue, 
      sigma_cuadrado = input$sigma_cuadrado, 
      rho = input$rho, 
      v_max = input$v_max
    )
    output$resultados <- renderText({
      paste("Tratamientos:", resultados$num_de_tratamientos,
            "\nRéplicas por tratamiento:", resultados$num_de_replicas)
    })
  })
  
  # Calculo 4: Potencia
  observeEvent(input$calcular_4, {
    resultados <- calcular_potencia(
      r = input$r_potencia,
      t = input$t_potencia,
      sigma2 = input$sigma2_potencia,
      Delta = input$Delta_potencia,
      alpha = input$alpha_potencia
    )
    output$resultados <- renderPrint({
      resultados
    })
  })
  
  # Calculo 5: Harris-Hurvitz-Mood (HHM)
  observeEvent(input$calcular_5, {
    resultado <- calcular_r_HHM(
      S1 = input$S1_hhm,
      d = input$d_hhm,
      df2 = input$df2_hhm,
      K = input$K_hhm
    )
    output$resultados <- renderText({
      paste("Número estimado de réplicas (r):", round(resultado, 2))
    })
  })
}
