
# METODO DE TUKEY ---------------------------------------------------------

head_tukey <- "Cálculo del tamaño muestral por tratamiento según el método de Tukey.

Esta función permite estimar el número de réplicas necesarias por tratamiento en un diseño experimental, 
utilizando el método de Tukey para comparaciones múltiples. El objetivo es garantizar que, al aplicar pruebas 
post-hoc entre tratamientos, sea posible detectar una diferencia mínima significativa previamente establecida 
en la variable de interés. Si dicha diferencia no es detectada en el análisis, se asume que no se alcanzó 
el tamaño muestral requerido para evidenciarla estadísticamente."

# Numero de replicas en el modelo de efectos aleatorios -------------------

head_Efectos_Aleatorios <- 
  
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
          
          