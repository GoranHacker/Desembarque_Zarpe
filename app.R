# 1. Librerías y Temas ####
library(shiny)
library(shinythemes)
library(RODBC)
library(dplyr)
library(DT)
library(lubridate)
library(ggplot2)
library(plotly)
library(readxl)
library(tidyr)


# 2. Interfaz de Usuario (UI) ####
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Cambia el tema aquí
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;  # Cambia el color del fondo
      }
      .sidebar {
        background-color: #343a40;  # Cambia el color del fondo de la barra lateral
        color: #ffffff;  # Cambia el color del texto en la barra lateral
      }
      .sidebar h4, .sidebar label {
        color: #ffffff;  # Cambia el color del texto de los títulos y etiquetas en la barra lateral
      }
      .form-control {
        background-color: #ffffff;  # Cambia el color del fondo de los controles de formulario
        color: #000000;  # Cambia el color del texto en los controles de formulario
      }
    "))
  ),
  titlePanel("Resumen de Desembarques y Declaraciones"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Selecciona el rango de fechas:",
                     start = Sys.Date() - 30, end = Sys.Date()),
      checkboxGroupInput("especies", "Selecciona las especies:",
                         choices = list("Merluza Comun" = 242,
                                        "Reineta" = 274,
                                        "Jibia" = 445),
                         selected = c(242, 274, 445)),
      uiOutput("caleta_ui"),
      class = "sidebar"  # Aplica la clase CSS personalizada
    ),
    mainPanel(
      dataTableOutput("resumen_original"),
      plotlyOutput("desembarque_plot"),
      dataTableOutput("resumen_combinado"),
      plotlyOutput("line_plot"),

      downloadButton("download_data", "Descargar Datos")
    )
  )
)

# 3. Conexión a la Base de Datos y Obtención de Datos Iniciales ####
server <- function(input, output, session) {
  datos_iniciales <- reactive({
    sigcue <- odbcConnect(dsn = "Rquimera")
    on.exit(odbcClose(sigcue))
    
    # Cambiar el contexto a la base de datos SIGCUE
    invisible(sqlQuery(sigcue, "USE SIGCUE"))
    
    # Consulta para obtener los campos relevantes de la tabla SigcueDaDetalle
    consulta_sigcue_detalle <- "
    SELECT
      nrFolio,
      codEspecie,
      factorConversion,
      valorCaptura
    FROM
      SigcueDaDetalle
    WHERE
      codEspecie IN (242, 274, 445)
    "
    resultado_sigcue_detalle <- sqlQuery(sigcue, consulta_sigcue_detalle)
    if (nrow(resultado_sigcue_detalle) == 0) {
      stop("La consulta SigcueDaDetalle no devolvió resultados.")
    }
    resultado_sigcue_detalle <- resultado_sigcue_detalle %>%
      mutate(
        Nr_declaracion = nrFolio,
        Especie = case_when(
          codEspecie == 242 ~ "Merluza Comun",
          codEspecie == 274 ~ "Reineta",
          codEspecie == 445 ~ "Jibia",
          TRUE ~ NA_character_
        ),
        Desembarque = valorCaptura * factorConversion
      ) %>%
      select(Nr_declaracion, Especie, Desembarque, codEspecie)
    
    # Consulta para obtener los campos relevantes de la tabla SigcueDaEncabezadoENCTest
    consulta_sigcue_encabezado <- "
    SELECT
      Nr_Folio,
      Cd_Nave,
      Cd_PtoLlegada,
      Fc_Llegada
    FROM
      SigcueDaEncabezadoENCTest
    WHERE
      Fc_Llegada >= '2022-01-01' AND
      Cd_PtoLlegada IN (286, 268, 636)
    "
    resultado_sigcue_encabezado <- sqlQuery(sigcue, consulta_sigcue_encabezado)
    if (nrow(resultado_sigcue_encabezado) == 0) {
      stop("La consulta SigcueDaEncabezadoENCTest no devolvió resultados.")
    }
    resultado_sigcue_encabezado <- resultado_sigcue_encabezado %>%
      mutate(
        Nr_declaracion = Nr_Folio,
        Caleta = case_when(
          Cd_PtoLlegada == 286 ~ "CURANIPE",
          Cd_PtoLlegada == 268 ~ "DUAO",
          Cd_PtoLlegada == 636 ~ "MAGUILLINES",
          TRUE ~ NA_character_
        ),
        Fecha_Desembarque = as.Date(Fc_Llegada, format = "%Y-%m-%d")
      ) %>%
      select(Nr_declaracion, Caleta, Fecha_Desembarque)
    
    # Unir las dos tablas por Nr_declaracion
    resultado_combinado <- resultado_sigcue_detalle %>%
      inner_join(resultado_sigcue_encabezado, by = "Nr_declaracion")
    
    resultado_combinado
  })
# 4. Lectura y Procesamiento de Datos del Excel ####
  datos_zarpes <- reactive({
    url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQahlciP-WSB9_Jz4q9zlgIudVLthF4A90ZGF1b1YGRsLr6gulzweAQXgPhFotQ4WKT2C76Jy3zibe2/pub?output=xlsx"
    temp_file <- tempfile(fileext = ".xlsx")
    download.file(url, destfile = temp_file, mode = "wb")
    zarpes_data <- read_excel(temp_file)
    file.remove(temp_file)
    
    zarpes_data <- zarpes_data %>%
      mutate(
        Fecha = as.Date(FECHA, format = "%m/%d/%Y")
      ) %>%
      select(Fecha, Caleta = CALETA, Zarpes = NUMERO_EMBARCACIONES)
    
    zarpes_data
  })
  # 5. Generación de la UI para la Selección de Caletas ####
  output$caleta_ui <- renderUI({
    caletas <- unique(datos_iniciales()$Caleta)
    checkboxGroupInput("caletas", "Selecciona las caletas:", choices = caletas, selected = caletas)
  })
  
  # 6. Filtrado y Procesamiento de Datos Según las Selecciones del Usuario ####
  datos_filtrados <- reactive({
    datos <- datos_iniciales()
    
    # Filtrar por el rango de fechas seleccionado
    datos <- datos %>%
      filter(Fecha_Desembarque >= input$date_range[1] & Fecha_Desembarque <= input$date_range[2])
    
    # Filtrar por especies seleccionadas
    if (!is.null(input$especies)) {
      datos <- datos %>%
        filter(codEspecie %in% input$especies)
    }
    
    # Filtrar por caletas seleccionadas
    if (!is.null(input$caletas)) {
      datos <- datos %>%
        filter(Caleta %in% input$caletas)
    }
    
    datos
  })
  
  # 7. Combinación de Datos de la Base de Datos y del Excel ####
  datos_combinados <- reactive({
    datos_db <- datos_filtrados()
    datos_excel <- datos_zarpes()
    
    datos_combinados <- datos_db %>%
      inner_join(datos_excel, by = c("Caleta" = "Caleta", "Fecha_Desembarque" = "Fecha"))
    
    datos_combinados
  })
  
  # 8. Creación y Renderización de la Tabla Original ####
  output$resumen_original <- renderDataTable({
    resumen_final <- datos_filtrados() %>%
      group_by(Caleta, Especie) %>%
      summarise(
        Total_Desembarque = sum(Desembarque, na.rm = TRUE),
        Total_Declaraciones = n(),
        .groups = 'drop'
      )
    
    datatable(
      resumen_final,
      caption = "Resumen de Desembarques y Declaraciones por Especie y Caleta",
      extensions = 'Buttons',
      options = list(
        pageLength = 10, 
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn btn-primary'),
          list(extend = 'csv', className = 'btn btn-primary'),
          list(extend = 'excel', className = 'btn btn-primary'),
          list(extend = 'pdf', className = 'btn btn-primary'),
          list(extend = 'print', className = 'btn btn-primary')
        )
      )
    )
  })
  
  # 9. Creación y Renderización de la Tabla Combinada ####
  output$resumen_combinado <- renderDataTable({
    resumen_combinado <- datos_combinados() %>%
      group_by(Fecha_Desembarque, Caleta) %>%
      summarise(
        Zarpes = max(Zarpes, na.rm = TRUE),
        `Declaraciones Merluza` = sum(ifelse(Especie == "Merluza Comun", 1, 0), na.rm = TRUE),
        `Declaraciones Jibia` = sum(ifelse(Especie == "Jibia", 1, 0), na.rm = TRUE),
        `Declaraciones Reineta` = sum(ifelse(Especie == "Reineta", 1, 0), na.rm = TRUE),
        .groups = 'drop'
      )
    
    datatable(
      resumen_combinado,
      caption = "Resumen Combinado de Zarpes y Declaraciones por Caleta",
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn btn-primary'),
          list(extend = 'csv', className = 'btn btn-primary'),
          list(extend = 'excel', className = 'btn btn-primary'),
          list(extend = 'pdf', className = 'btn btn-primary'),
          list(extend = 'print', className = 'btn btn-primary')
        )
      )
    )
  })
  
  # 10. Creación y Renderización del Gráfico de Desembarques por Especie ####
  output$desembarque_plot <- renderPlotly({
    plot_data <- datos_filtrados() %>%
      group_by(Fecha_Desembarque, Especie) %>%
      summarise(Total_Desembarque = sum(Desembarque, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = Especie, values_from = Total_Desembarque, values_fill = list(Total_Desembarque = 0))
    
    plot_data <- plot_data %>%
      pivot_longer(cols = -Fecha_Desembarque, names_to = "Especie", values_to = "Total_Desembarque")
    
    p <- ggplot(plot_data, aes(x = Fecha_Desembarque, y = Total_Desembarque, color = Especie)) +
      geom_line(size = 1) +
      labs(
        title = "Desembarque por Fecha para Cada Pesquería",
        x = "Fecha",
        y = "Desembarque"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 11. Creación y Renderización del Gráfico de Zarpes y Declaraciones ####
  output$line_plot <- renderPlotly({
    resumen_combinado <- datos_combinados() %>%
      group_by(Fecha_Desembarque, Caleta) %>%
      summarise(
        Zarpes = max(Zarpes, na.rm = TRUE),  # Usar max en vez de sum para evitar duplicados
        `Declaraciones Merluza` = sum(ifelse(Especie == "Merluza Comun", 1, 0), na.rm = TRUE),
        `Declaraciones Jibia` = sum(ifelse(Especie == "Jibia", 1, 0), na.rm = TRUE),
        `Declaraciones Reineta` = sum(ifelse(Especie == "Reineta", 1, 0), na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_data <- resumen_combinado %>%
      group_by(Fecha_Desembarque) %>%
      summarise(
        Zarpes = sum(Zarpes, na.rm = TRUE),
        `Merluza Comun` = sum(`Declaraciones Merluza`, na.rm = TRUE),
        Jibia = sum(`Declaraciones Jibia`, na.rm = TRUE),
        Reineta = sum(`Declaraciones Reineta`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = c(`Merluza Comun`, Jibia, Reineta, Zarpes), names_to = "Categoria", values_to = "Cantidad")
    
    p <- ggplot(plot_data, aes(x = Fecha_Desembarque, y = Cantidad, color = Categoria)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("Zarpes" = "red", "Merluza Comun" = "blue", "Jibia" = "green", "Reineta" = "orange")) +
      labs(
        title = "Zarpes y Declaraciones por Fecha",
        x = "Fecha",
        y = "Cantidad"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 12. Función para Descargar los Datos Filtrados ####
  output$download_data <- downloadHandler(
    filename = function() {
      paste("resumen_desembarques_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_combinados(), file, row.names = FALSE)
    }
  )
  
}
# 13. Ejecución de la Aplicación ####
shinyApp(ui = ui, server = server)
