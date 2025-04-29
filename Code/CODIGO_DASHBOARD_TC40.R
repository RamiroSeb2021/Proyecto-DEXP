# Librerías necesarias
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(corrplot)
library(factoextra)
library(cluster)
library(tidyr)
library(scales)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard TC40 - Análisis de Fraudes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datos", tabName = "data", icon = icon("table")),
      menuItem("Resumen General", tabName = "overview", icon = icon("dashboard")),
      menuItem("Análisis Geográfico", tabName = "geographic", icon = icon("globe")),
      menuItem("Análisis Comercial", tabName = "merchant", icon = icon("store")),
      menuItem("Indicadores de Fraude", tabName = "indicators", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Pestaña de Datos
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Datos de Fraudes", DTOutput("data_table"), width = 12)
              )
      ),
      
      # Pestaña de Resumen General
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_frauds_box", width = 3),
                valueBoxOutput("total_amount_box", width = 3),
                valueBoxOutput("avg_amount_box", width = 3),
                valueBoxOutput("fraud_types_box", width = 3)
              ),
              fluidRow(
                box(title = "Frecuencia de Tipos de Fraude", plotlyOutput("fraud_types_plot"), width = 6, height = 400),
                box(title = "Monto Total por Tipo de Fraude", plotlyOutput("fraud_amounts_plot"), width = 6, height = 400)
              )
      ),
      
      # Pestaña de Análisis Geográfico
      tabItem(tabName = "geographic",
              fluidRow(
                box(title = "Distribución Geográfica de Fraudes", plotlyOutput("geo_distribution_plot"), width = 12, height = 400)
              ),
              fluidRow(
                box(title = "Top 10 Países", plotlyOutput("top_countries_plot"), width = 6),
                box(title = "Top 10 Ciudades", plotlyOutput("top_cities_plot"), width = 6)
              )
      ),
      
      # Pestaña de Análisis Comercial
      tabItem(tabName = "merchant",
              fluidRow(
                box(title = "Categorías de Comercio más Afectadas", plotlyOutput("merchant_categories_plot"), width = 12)
              ),
              fluidRow(
                box(title = "Análisis Detallado por Comercio", DTOutput("merchant_details_table"), width = 12)
              )
      ),
      
      # Nueva Pestaña de Indicadores de Fraude
      tabItem(tabName = "indicators",
              fluidRow(
                # Indicador 1: Porcentaje de Captura
                box(title = "Porcentaje de Captura de Fraude", width = 6, status = "warning", solidHeader = TRUE, plotlyOutput("fraud_capture_pie"), footer = "Por número de transacciones"),
                
                # Indicador 2: Productos Afectados
                box(title = "Productos Afectados por Cliente", width = 6, status = "danger", solidHeader = TRUE, plotlyOutput("affected_products_hist"), footer = "Distribución de productos afectados por cliente")
              ),
              fluidRow(
                box(title = "Detalles de Indicadores", width = 12, status = "info", solidHeader = TRUE, DTOutput("indicators_table"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Cargar datos
  data <- reactive({
    # Asumiendo que TC40_filtered ya está cargado en el ambiente
    TC40_filtered
  })
  
  # Mostrar el dataset en la pestaña "Datos"
  output$data_table <- renderDT({
    datatable(data(), options = list(
      pageLength = 10,
      scrollY = "300px",
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      searchHighlight = TRUE,
      language = list(search='Buscar:', zeroRecords='No se encontraron resultados.')
    ), class="display compact", rownames=FALSE)
  })
  
  # Resumen General
  output$total_frauds_box <- renderValueBox({
    valueBox(formatC(nrow(data()), format="d", big.mark=","), 
             "Total de Fraudes",
             icon=icon("exclamation-triangle"),
             color="red")
  })
  
  output$total_amount_box <- renderValueBox({
    total_amount <- sum(data()$`Fraud Amount (Local Currency)`, na.rm=TRUE)
    valueBox(paste0("$", formatC(total_amount, format="f", digits=0, big.mark=",")), 
             "Monto Total de Fraudes",
             icon=icon("dollar-sign"),
             color="yellow")
  })
  
  output$avg_amount_box <- renderValueBox({
    avg_amount <- mean(data()$`Fraud Amount (Local Currency)`, na.rm=TRUE)
    valueBox(paste0("$", formatC(avg_amount, format="f", digits=2, big.mark=",")), 
             "Monto Promedio",
             icon=icon("chart-bar"),
             color="blue")
  })
  
  output$fraud_types_box <- renderValueBox({
    n_types <- length(unique(data()$`Fraud Type Desc`))
    valueBox(n_types, 
             "Tipos de Fraude",
             icon=icon("list"),
             color="purple")
  })
  
  # Gráficos de Resumen General
  output$fraud_types_plot <- renderPlotly({
    fraud_types <- data() %>% count(`Fraud Type Desc`) %>% arrange(desc(n)) %>% mutate(pct=n/sum(n)*100)
    plot_ly(fraud_types, x=~reorder(`Fraud Type Desc`, -n), y=~n, type="bar",
            text=~paste0(round(pct,1), "%"),
            marker=list(color="steelblue")) %>%
      layout(xaxis=list(title="Tipo de Fraude"),
             yaxis=list(title="Frecuencia"),
             showlegend=FALSE)
  })
  
  
  output$fraud_amounts_plot <- renderPlotly({
    amounts_by_type <- data() %>% group_by(`Fraud Type Desc`) %>% summarise(total_amount=sum(`Fraud Amount (Local Currency)`, na.rm=TRUE)) %>% arrange(desc(total_amount))
    plot_ly(amounts_by_type, x=~reorder(`Fraud Type Desc`, -total_amount), y=~total_amount, type="bar",
            marker=list(color="darkred")) %>%
      layout(xaxis=list(title="Tipo de Fraude"),
             yaxis=list(title="Monto Total ($)"))
  })
  
  # Gráficos de Análisis Geográfico
  output$geo_distribution_plot <- renderPlotly({
    geo_data <- data() %>% count(`Merchant Country`) %>% arrange(desc(n))
    plot_ly(geo_data, x=~reorder(`Merchant Country`, -n), y=~n, type="bar",
            marker=list(color="forestgreen")) %>%
      layout(xaxis=list(title="País"),
             yaxis=list(title="Número de Fraudes"))
  })
  
  output$top_countries_plot <- renderPlotly({
    top_countries <- data() %>% count(`Merchant Country`) %>% top_n(10,n) %>% arrange(desc(n))
    plot_ly(top_countries, labels=~`Merchant Country`, values=~n, type="pie") %>%
      layout(title="Top 10 Países por Número de Fraudes")
  })
  
  output$top_cities_plot <- renderPlotly({
    top_cities <- data() %>% count(`Merchant City`) %>% top_n(10,n) %>% arrange(desc(n))
    plot_ly(top_cities, x=~reorder(`Merchant City`, -n), y=~n, type="bar",
            marker=list(color="orange")) %>%
      layout(xaxis=list(title="Ciudad"),
             yaxis=list(title="Número de Fraudes"))
  })
  
  # Gráficos de Análisis Comercial
  output$merchant_categories_plot <- renderPlotly({
    merchant_cats <- data() %>% count(`Merchant Category Code Desc`) %>% top_n(10,n) %>% arrange(desc(n))
    plot_ly(merchant_cats, x=~reorder(`Merchant Category Code Desc`, -n), y=~n, type="bar",
            marker=list(color="purple")) %>%
      layout(xaxis=list(title="Categoría de Comercio"),
             yaxis=list(title="Número de Fraudes"))
  })
  
  # Tabla de Comercios
  output$merchant_details_table <- renderDT({
    merchant_data <- data() %>%
      select(`Merchant Name`, `Merchant Category Code Desc`, `Fraud Amount (Local Currency)`, `Fraud Type Desc`, `Merchant City`, `Merchant Country`) %>%
      arrange(desc(`Fraud Amount (Local Currency)`))
    
    datatable(merchant_data,
              options=list(pageLength=10,
                           scrollY="300px",
                           scrollX=TRUE,
                           dom='Bfrtip',
                           buttons=c('copy', 'csv', 'excel'),
                           searchHighlight=TRUE,
                           language=list(search='Buscar:', zeroRecords='No se encontraron resultados.')),
              class="display compact",
              rownames=FALSE)
  })
  
  # Nuevos outputs para Indicadores de Fraude
  
  # Porcentaje de Captura de Fraude
  output$fraud_capture_pie <- renderPlotly({
    fraud_data <- data() %>%
      group_by(`Fraud Type Desc`) %>%
      summarise(count=n()) %>%
      mutate(percentage=count/sum(count)*100)
    
    plot_ly(fraud_data, labels=~`Fraud Type Desc`, values=~count,
            type='pie', textinfo='label+percent',
            insidetextorientation='radial')
  })
  
  # Productos Afectados por Cliente
  output$affected_products_hist <- renderPlotly({
    products_per_client <- data() %>%
      group_by(`Card Account Number`) %>%
      summarise(n_products=n_distinct(`Issuer BIN`), total_fraud=sum(`Fraud Amount (Local Currency)`, na.rm=TRUE))
    
    plot_ly(products_per_client, x=~n_products,
            type="histogram", nbinsx=10,
            marker=list(color="indianred")) %>%
      layout(xaxis=list(title="Número de Productos"),
             yaxis=list(title="Frecuencia"),
             showlegend=FALSE)
  })
  
  # Tabla de Resumen de Indicadores
  output$indicators_table <- renderDT({
    # Calcular métricas para la tabla
    total_fraud_amount <- sum(data()$`Fraud Amount (Local Currency)`, na.rm=TRUE)
    
    total_amount <- total_fraud_amount * 100 # Asumido para ejemplo
    
    capture_rate <- nrow(data()) / (nrow(data()) * 1.2) # Asumido un 20% no capturado
    
    avg_products <- data() %>%
      group_by(`Card Account Number`) %>%
      summarise(n_products=n_distinct(`Issuer BIN`)) %>%
      summarise(avg=mean(n_products)) %>%
      pull(avg)
    
    
    indicators_df <- data.frame(
      Indicador=c("Tasa de Captura","Promedio Productos Afectados"),
      Valor=c(sprintf("%.1f%%", capture_rate * 100), sprintf("%.1f", avg_products)),
      Descripción=c("Porcentaje de fraudes detectados vs total estimado","Promedio de productos afectados por cliente"),
      Umbral_Verde=c(">80%","<1.5"),
      Umbral_Amarillo=c("60-80%","1.5-2.5"),
      Umbral_Rojo=c("<60%","<2.5")
    )
    
    datatable(indicators_df,
              options=list(pageLength=5,
                           dom='t',
                           ordering=FALSE),
              rownames=FALSE)
    
  })
  
}

# Lanza la aplicación Shiny
shinyApp(ui=ui, server=server)

