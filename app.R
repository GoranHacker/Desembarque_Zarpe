# 1. Librerías y Temas ####

# Cargar librerías necesarias
library(shiny)          # Librería principal para crear aplicaciones web interactivas
library(shinythemes)    # Librería para aplicar temas predefinidos a la interfaz de usuario
library(RODBC)          # Librería para la conexión a bases de datos ODBC
library(dplyr)          # Librería para manipulación de datos
library(DT)             # Librería para renderizar tablas interactivas
library(lubridate)      # Librería para trabajar con fechas y horas
library(ggplot2)        # Librería para crear gráficos
library(plotly)         # Librería para hacer gráficos interactivos
library(readxl)         # Librería para leer archivos Excel
library(tidyr)          # Librería para la manipulación y limpieza de datos

# 2. Interfaz de Usuario (UI) ####

ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Cambia el tema de la aplicación a 'cerulean'
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;  # Cambia el color del fondo del cuerpo de la página
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
  
  titlePanel("Resumen de Zarpe y Desembarque"),  # Título de la aplicación
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Selecciona el rango de fechas:",
                     start = Sys.Date() - 30, end = Sys.Date()),  # Selector de rango de fechas
      checkboxGroupInput("especies", "Selecciona las especies:",
                         choices = list("Merluza Comun" = 242,
                                        "Reineta" = 274,
                                        "Jibia" = 445),
                         selected = c(242, 274, 445)),  # Selector de especies
      uiOutput("caleta_ui"),  # Salida dinámica para la selección de caletas
      class = "sidebar"  # Aplica la clase CSS personalizada
    ),
    
    mainPanel(
      dataTableOutput("resumen_original"),  # Tabla de resumen original
      plotlyOutput("desembarque_plot"),  # Gráfico de desembarques
      dataTableOutput("resumen_combinado"),  # Tabla de resumen combinado
      plotlyOutput("line_plot")  # Gráfico de líneas
    )
  )
)

# 3. Conexión a la Base de Datos y Obtención de Datos Iniciales ####
server <- function(input, output, session) {
  datos_iniciales <- reactive({
    # Conectar a la base de datos usando ODBC
    sigcue <- odbcConnect(dsn = "Rquimera") # Establece la conexión a la base de datos.
    on.exit(odbcClose(sigcue))  # Asegurarse de cerrar la conexión al final
    
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
    resultado_sigcue_detalle <- sqlQuery(sigcue, consulta_sigcue_detalle) # Ejecuta una consulta SQL para obtener los datos iniciales de la tabla SigcueDaDetalle.
    if (nrow(resultado_sigcue_detalle) == 0) {
      stop("La consulta SigcueDaDetalle no devolvió resultados.")
    }
    
    # Procesar los datos obtenidos
    resultado_sigcue_detalle <- resultado_sigcue_detalle %>%
      mutate( # Transforman los datos obtenidos, creando nuevas columnas y asignando valores basados en condiciones.
        Nr_declaracion = nrFolio,
        Especie = case_when( # Transforman los datos obtenidos, creando nuevas columnas y asignando valores basados en condiciones.
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
    
    # Procesar los datos obtenidos
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
      inner_join(resultado_sigcue_encabezado, by = "Nr_declaracion") # Une dos tablas de datos basadas en una columna común (Nr_declaracion).
    
    resultado_combinado
  })
  
  # 4. Lectura y Procesamiento de Datos del Excel ####
  datos_zarpes <- reactive({
    # URL del archivo Excel alojado en Google Sheets
    url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQahlciP-WSB9_Jz4q9zlgIudVLthF4A90ZGF1b1YGRsLr6gulzweAQXgPhFotQ4WKT2C76Jy3zibe2/pub?output=xlsx" # URL del archivo Excel alojado en Google Sheets.
    
    # Crear un archivo temporal para descargar el Excel
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Descargar el archivo desde la URL
    download.file(url, destfile = temp_file, mode = "wb") 
    
    # Leer los datos del archivo Excel descargado
    zarpes_data <- read_excel(temp_file)
    
    # Eliminar el archivo temporal una vez leído
    file.remove(temp_file)
    
    # Procesar los datos del Excel
    zarpes_data <- zarpes_data %>%
      mutate(
        # Convertir la columna FECHA a formato Date
        Fecha = as.Date(FECHA, format = "%m/%d/%Y")
      ) %>%
      # Seleccionar y renombrar las columnas relevantes
      select(Fecha, Caleta = CALETA, Zarpes = NUMERO_EMBARCACIONES)
    
    # Retornar los datos procesados
    zarpes_data
  })
  
  
  # 5. Generación de la UI para la Selección de Caletas ####
  output$caleta_ui <- renderUI({
    # Obtiene la lista única de caletas de los datos iniciales
    caletas <- unique(datos_iniciales()$Caleta)
    # Genera un grupo de checkboxes para seleccionar las caletas
    checkboxGroupInput(
      inputId = "caletas",  # ID del input
      label = "Selecciona las caletas:",  # Etiqueta del input
      choices = caletas,  # Opciones de caletas disponibles
      selected = caletas  # Opciones seleccionadas por defecto
    )
  })
  
  # 6. Filtrado y Procesamiento de Datos Según las Selecciones del Usuario ####
  datos_filtrados <- reactive({
    # Obtiene los datos iniciales
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
    
    # Retornar los datos filtrados
    datos
  })
  
  # 7. Combinación de Datos de la Base de Datos y del Excel ####
  datos_combinados <- reactive({
    # Obtiene los datos filtrados de la base de datos
    datos_db <- datos_filtrados()
    
    # Obtiene los datos del Excel
    datos_excel <- datos_zarpes()
    
    # Crear un marco de datos con todas las fechas y todas las caletas
    unique_caletas <- unique(c(datos_db$Caleta, datos_excel$Caleta))  # Lista única de caletas
    all_dates <- seq.Date(from = input$date_range[1], to = input$date_range[2], by = "day")  # Rango de todas las fechas
    all_dates_caletas_df <- expand.grid(Fecha_Desembarque = all_dates, Caleta = unique_caletas)  # Crear todas las combinaciones de fechas y caletas
    
    # Unir datos del Excel con la base de datos usando full_join
    datos_combinados <- all_dates_caletas_df %>%
      left_join(datos_excel, by = c("Fecha_Desembarque" = "Fecha", "Caleta" = "Caleta")) %>%
      left_join(datos_db, by = c("Fecha_Desembarque", "Caleta")) %>%
      replace_na(list(Zarpes = 0, `Declaraciones Merluza` = 0, `Declaraciones Jibia` = 0, `Declaraciones Reineta` = 0))  # Reemplazar valores NA con 0
    
    # Retornar los datos combinados
    datos_combinados
  })
  
  # 8. Creación y Renderización de la Tabla Original ####
  output$resumen_original <- renderDataTable({
    # Agrupar los datos filtrados por caleta y especie y resumir los desembarques y declaraciones
    resumen_final <- datos_filtrados() %>%
      group_by(Caleta, Especie) %>%
      summarise(
        Total_Desembarque = sum(Desembarque, na.rm = TRUE),  # Sumar el total de desembarques
        Total_Declaraciones = n(),  # Contar el número de declaraciones
        .groups = 'drop'
      )
    
    # Renderizar la tabla de resumen final
    datatable(
      resumen_final,
      caption = "Resumen de Desembarques y Declaraciones por Especie y Caleta",  # Título de la tabla
      extensions = 'Buttons',
      options = list(
        pageLength = 10,  # Número de filas por página
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn btn-primary'),  # Botón para copiar
          list(extend = 'csv', className = 'btn btn-primary'),  # Botón para descargar CSV
          list(extend = 'excel', className = 'btn btn-primary'),  # Botón para descargar Excel
          list(extend = 'pdf', className = 'btn btn-primary'),  # Botón para descargar PDF
          list(extend = 'print', className = 'btn btn-primary')  # Botón para imprimir
        )
      )
    )
  })
  
  # 9. Creación y Renderización de la Tabla Combinada ####
  output$resumen_combinado <- renderDataTable({
    # Agrupar los datos combinados por fecha de desembarque, caleta y zarpes, y resumir las declaraciones por especie
    resumen_combinado <- datos_combinados() %>%
      group_by(Fecha_Desembarque, Caleta, Zarpes) %>%
      summarise(
        `Declaraciones Merluza` = sum(ifelse(Especie == "Merluza Comun", 1, 0), na.rm = TRUE),  # Sumar las declaraciones de Merluza
        `Declaraciones Jibia` = sum(ifelse(Especie == "Jibia", 1, 0), na.rm = TRUE),  # Sumar las declaraciones de Jibia
        `Declaraciones Reineta` = sum(ifelse(Especie == "Reineta", 1, 0), na.rm = TRUE),  # Sumar las declaraciones de Reineta
        .groups = 'drop'
      )
    
    # Renderizar la tabla de resumen combinado
    datatable(
      resumen_combinado,
      caption = "Resumen Combinado de Zarpes y Declaraciones por Caleta",  # Título de la tabla
      extensions = 'Buttons',
      options = list(
        pageLength = 10,  # Número de filas por página
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn btn-primary'),  # Botón para copiar
          list(extend = 'csv', className = 'btn btn-primary'),  # Botón para descargar CSV
          list(extend = 'excel', className = 'btn btn-primary'),  # Botón para descargar Excel
          list(extend = 'pdf', className = 'btn btn-primary'),  # Botón para descargar PDF
          list(extend = 'print', className = 'btn btn-primary')  # Botón para imprimir
        ),
        scrollY = '400px',  # Habilitar scroll vertical
        scrollCollapse = TRUE,  # Permitir que la tabla se colapse cuando no quepa en la pantalla
        paging = FALSE  # Deshabilitar paginación
      )
    )
  })
  # 10. Creación y Renderización del Gráfico de Desembarques por Especie ####
  output$desembarque_plot <- renderPlotly({
    # Agrupar los datos filtrados por fecha de desembarque y especie, y sumar los desembarques
    plot_data <- datos_filtrados() %>%
      group_by(Fecha_Desembarque, Especie) %>%
      summarise(Total_Desembarque = sum(Desembarque, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = Especie, values_from = Total_Desembarque, values_fill = list(Total_Desembarque = 0))
    
    # Transformar los datos a un formato largo para ggplot
    plot_data <- plot_data %>%
      pivot_longer(cols = -Fecha_Desembarque, names_to = "Especie", values_to = "Total_Desembarque")
    
    # Formatear la fecha
    plot_data$Fecha_Desembarque <- format(plot_data$Fecha_Desembarque, "%d-%m-%Y")
    
    # Crear el gráfico de líneas con ggplot
    p <- ggplot(plot_data, aes(x = as.Date(Fecha_Desembarque, "%d-%m-%Y"), y = Total_Desembarque, color = Especie)) +
      geom_line(size = 1) +
      labs(
        title = "Desembarque por Fecha para Cada Pesquería",  # Título del gráfico
        x = "Fecha",  # Etiqueta del eje X
        y = "Desembarque"  # Etiqueta del eje Y
      ) +
      theme_minimal() +
      scale_x_date(labels = function(x) format(x, "%d-%b"))  # Formato de fecha en el eje X
    
    # Convertir el gráfico a un objeto interactivo con ggplotly
    ggplotly(p) %>%
      layout(xaxis = list(tickformat = "%d-%m-%Y"))  # Formato de fecha al pasar el mouse
  })
  
  # 11. Creación y Renderización del Gráfico de Zarpes y Declaraciones ####
  output$line_plot <- renderPlotly({
    # Agrupar los datos combinados por fecha de desembarque y caleta, y resumir los zarpes y declaraciones
    resumen_combinado <- datos_combinados() %>%
      group_by(Fecha_Desembarque, Caleta) %>%
      summarise(
        Zarpes = max(Zarpes, na.rm = TRUE),  # Usar max en vez de sum para evitar duplicados
        `Declaraciones Merluza` = sum(ifelse(Especie == "Merluza Comun", 1, 0), na.rm = TRUE),  # Sumar las declaraciones de Merluza
        `Declaraciones Jibia` = sum(ifelse(Especie == "Jibia", 1, 0), na.rm = TRUE),  # Sumar las declaraciones de Jibia
        `Declaraciones Reineta` = sum(ifelse(Especie == "Reineta", 1, 0), na.rm = TRUE),  # Sumar las declaraciones de Reineta
        .groups = 'drop'
      )
    
    # Agrupar los datos resumidos por fecha de desembarque y sumar las categorías
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
    
    # Formatear la fecha
    plot_data <- plot_data %>%
      mutate(Fecha_Desembarque = format(Fecha_Desembarque, "%d-%m-%Y"))
    
    # Crear el gráfico de líneas con ggplot
    p <- ggplot(plot_data, aes(x = as.Date(Fecha_Desembarque, "%d-%m-%Y"), y = Cantidad, color = Categoria)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("Zarpes" = "red", "Merluza Comun" = "blue", "Jibia" = "green", "Reineta" = "orange")) +
      labs(
        title = "Zarpes y Declaraciones por Fecha",  # Título del gráfico
        x = "Fecha",  # Etiqueta del eje X
        y = "Cantidad"  # Etiqueta del eje Y
      ) +
      theme_minimal() +
      scale_x_date(labels = scales::date_format("%d-%b"))  # Formato de fecha en el eje X
    
    # Convertir el gráfico a un objeto interactivo con ggplotly
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  # 12. Función para Descargar los Datos Filtrados ####
  output$download_data <- downloadHandler(
    filename = function() {
      paste("resumen_desembarques_", Sys.Date(), ".csv", sep = "")  # Nombre del archivo de salida
    },
    content = function(file) {
      # Escribir los datos combinados en un archivo CSV
      write.csv(datos_combinados(), file, row.names = FALSE)
    }
  )
}
# 13. Ejecución de la Aplicación ####
shinyApp(ui = ui, server = server)

