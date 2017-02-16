
library(shiny)
library(plotly)

shinyUI(fluidPage(
     
     titlePanel("Revisar los modelos que causan desviaciones en las lineas de produccion"),
     sidebarLayout(
          sidebarPanel(
               h5("Utiliza un archivo .csv (grabalo asi desde excel) con los estilos y la linea de
                  produccion a la que pertenece cada una. Con el formato que se muestra en la imagen"),
               img(src= "http://www.magro.com.mx/images/formato.PNG", align = "left",
                   width = 200),
               fileInput("browse", "Selecciona archivo CSV",
                         accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
               ),
               checkboxInput("header", "Datos tienen encabezado", TRUE),
               checkboxInput("agrupado", "Agrupar en una sola linea de produccion", FALSE),
               downloadButton("download","Descargar asignacion")
          ),
          mainPanel(
               # h5("Esta versión permite agrupar 20 estilos, si necesitas agrupar más puedes comprar
               #    la suscripción en Apps/Comprar aplicaciones o enviarnos un correo en la cuenta 
               #    luis@magro.com.mx para ayudarte"),
               h5("Si tienes alguna duda de como funciona esta app, puedes enviarnos un correo a 
                  luis@magro.com.mx para ayudarte"),
               tabsetPanel(
                    tabPanel("Datos leidos",DT::dataTableOutput("tabla_completa")),
                    tabPanel("Analisis de asignacion", 
                             #tableOutput("mejora"),
                             column(4,tableOutput("total.fam")),
                             column(12, p("Los siguientes graficos permiten entender la forma en que estan
                                          asignados los modelos a cada linea de produccion y revisar si la
                                          asignacion tiene errores, visualmente se puede verificar la 
                                          consistencia en la asignacion.")),
                             column(12,plotlyOutput("grafico.final", height = "1500px"))
                             ),
                    tabPanel("Analisis de desviaciones",
                             column(4,uiOutput("seleccion_linea")),
                             column(8,sliderInput("quant", "Limite para considerar como desviacion",
                                                  min = 0, max = 49, step = 1, value = 10)),
                             column(3,p("Desviacion al cancelar criticos"),
                                    verbatimTextOutput("nva.desviacion"),
                                    p("Porcentaje de mejora"),
                                    verbatimTextOutput("pct.mejora")),
                             column(3, p("Mayores causantes de desviacion"),
                                    tableOutput("cancelar.criticos")),
                             column(6, p("Indicadores de desviacion"),
                                    tableOutput("indicador.desviacion")),
                             checkboxInput("same.scale.fin", 
                                                    "Usar escala independiente en cada grafico", FALSE),
                             column(12,plotlyOutput("plot.por.linea", height = "800px")),
                             DT::dataTableOutput("desviaciones", width = 200)),
                    tabPanel("Personal Requerido",
                             column(4,sliderInput("horas.trabajo", "Horas trabajadas por dia",
                                         min = 1, max = 12, step = 0.5, value = 9.5)),
                             column(4,sliderInput("eficiencia", "Eficiencia de balanceo",
                                         min=10, max = 130, step = 5, value = 85)),
                             column(4,sliderInput("pares.hora","Pares a producir por dia",
                                         min=100, max = 10000, step = 50, value = 4000)),
                             column(3,tableOutput("Totales")),
                             column(3,tableOutput("total_puesto")),
                             column(2, p("Total personas requeridas"),
                                    verbatimTextOutput("grantotal")),
                             DT::dataTableOutput("Porlinea",width = 400)
                             )
               )
          )
     )
))
