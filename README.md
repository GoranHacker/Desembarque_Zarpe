# Desembarque_Zarpe
App Shiny con datos de desembarque y zarpe

Descripción de la Aplicación Shiny: Resumen de Zarpe y Desembarque
Introducción
Esta aplicación Shiny busca generar la base para futuras herramientas analíticas para Sernapesca, utilizando datos de desembarque de las bases de datos institucionales y datos de zarpes de la región del Maule. En primera instancia, se realiza un análisis descriptivo y comparativo. En el futuro, se espera que las unidades de análisis, en coordinación con la unidad de inteligencia, trabajen en la coconstrucción de nuevas y mejores herramientas analíticas avanzadas en R.

Funcionalidades
Selector de Rango de Fechas:
Permite a los usuarios seleccionar un rango de fechas específico para analizar los datos de desembarques y declaraciones.

Selector de Especies:
Los usuarios pueden seleccionar una o varias especies (Merluza Común, Reineta, Jibia) para filtrar los datos.

Selector de Caletas:
Los usuarios pueden seleccionar las caletas de interés para visualizar los datos correspondientes.

Tabla Resumen de Desembarques y Declaraciones:
Muestra un resumen de los desembarques y declaraciones por especie y caleta en el rango de fechas seleccionado.
Incluye botones para copiar, descargar (en formatos CSV, Excel, PDF) e imprimir la tabla.

Gráfico de Desembarques por Fecha:
Un gráfico interactivo que muestra los desembarques diarios para cada especie seleccionada en el rango de fechas especificado.

Tabla Resumen Combinado de Zarpes y Declaraciones:
Muestra un resumen combinado de zarpes y declaraciones por caleta, incluyendo los días en que no hubo desembarques.
Incluye botones para copiar, descargar (en formatos CSV, Excel, PDF) e imprimir la tabla.
La tabla tiene scroll vertical para facilitar la navegación.

Gráfico de Zarpes y Declaraciones por Fecha:
Un gráfico interactivo que muestra la cantidad de zarpes y declaraciones diarias para cada especie seleccionada en el rango de fechas especificado.

Cómo Utilizar la Aplicación:

1.	Configuración Inicial:
Asegúrese de tener instaladas todas las librerías necesarias: shiny, shinythemes, RODBC, dplyr, DT, lubridate, ggplot2, plotly, readxl, tidyr.

2.	Ejecutar la Aplicación:
Ejecute el script app.R en su entorno de RStudio.
La aplicación se abrirá en su navegador web predeterminado.

3.	Interacción con la Aplicación:
Use los selectores de rango de fechas, especies y caletas para filtrar los datos.
Explore las tablas y gráficos generados.
Use los botones de descarga para exportar los datos en el formato deseado.

Contenido del Script:
Sección 1: Librerías y Temas
Esta sección se encarga de cargar las librerías necesarias y definir el tema que se utilizará en la interfaz de usuario de la aplicación Shiny.

Sección 2: Interfaz de Usuario (UI)
Esta sección define la interfaz de usuario (UI) de la aplicación Shiny. Utiliza fluidPage para crear un diseño fluido y sidebarLayout para organizar la interfaz con una barra lateral (sidebar) y un panel principal (main panel).

Sección 3: Conexión a la Base de Datos y Obtención de Datos Iniciales
Esta sección establece la conexión con la base de datos y obtiene los datos iniciales que se utilizarán en la aplicación.

Sección 4: Lectura y Procesamiento de Datos del Excel
Esta sección se encarga de leer los datos del Excel desde una URL y procesarlos para su posterior uso en la aplicación.

Sección 5: Generación de la UI para la Selección de Caletas
Esta sección se encarga de generar dinámicamente la interfaz de usuario para la selección de caletas.

Sección 6: Filtrado y Procesamiento de Datos Según las Selecciones del Usuario
Esta sección se encarga de filtrar y procesar los datos en función de las selecciones realizadas por el usuario en la interfaz de usuario.

Sección 7: Combinación de Datos de la Base de Datos y del Excel
Esta sección se encarga de combinar los datos obtenidos de la base de datos y del archivo Excel, asegurando que todas las fechas y caletas estén representadas.

Sección 8: Creación y Renderización de la Tabla Original
Esta sección se encarga de crear y renderizar la tabla original que muestra el resumen de desembarques y declaraciones por especie y caleta.

Sección 9: Creación y Renderización de la Tabla Combinada
Esta sección se encarga de crear y renderizar la tabla combinada que muestra el resumen de zarpes y declaraciones por caleta.

Sección 10: Creación y Renderización del Gráfico de Desembarques por Especie
Esta sección se encarga de crear y renderizar un gráfico de líneas que muestra los desembarques por fecha para cada pesquería.

Sección 11: Creación y Renderización del Gráfico de Zarpes y Declaraciones
Esta sección se encarga de crear y renderizar un gráfico de líneas que muestra los zarpes y las declaraciones por fecha.

Sección 12: Función para Descargar los Datos Filtrados
Esta sección se encarga de definir la funcionalidad para descargar los datos filtrados en un archivo CSV.

Sección 13: Ejecución de la Aplicación
Esta sección se encarga de ejecutar la aplicación Shiny.


