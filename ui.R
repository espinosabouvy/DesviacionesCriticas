
library(shiny)
library(plotly)

shinyUI(fluidPage(
     
     titlePanel("Diagnostico, analisis y mejoras de lineas de produccion - TU FABRICA"),
     #titlePanel("Diagnostico, analisis y mejoras de lineas de produccion - PERUGIA"),
     sidebarLayout(
          sidebarPanel(
               fileInput("browse", "Selecciona archivo CSV que obtienes de Tiempos/Reportes/Plantilla basica/
                         Estilos habilitados",
                         accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
               ),
               uiOutput("depto.select"),
               uiOutput("fams.select"),
               checkboxInput("agrupado", "Agrupar en una sola linea de produccion", FALSE),
               checkboxInput("personas", "Convertir tiempos a personas (datos en segundos)", FALSE),
               h3("Configuracion general"),
               sliderInput("horas.trabajo", "Horas trabajadas por dia",
                           min = 1, max = 12, step = 0.5, value = 9.5),
               sliderInput("eficiencia", "Eficiencia de balanceo",
                           min=10, max = 130, step = 5, value = 85),
               sliderInput("precio.prom", "Precio de venta promedio",
                           min = 50, max = 500, step = 10, value = 300, width = 400),
               h3("Pares por producir por dia por linea"),
               uiOutput("lineas.selected"),
               textInput("pares","Pares"),
               actionButton("agregar","Agregar..."),
               DT::dataTableOutput("por.producir")
          ),
          mainPanel(
               # h5("Esta version permite agrupar 20 estilos, si necesitas agrupar mas puedes comprar
               #    la suscripcion en Apps/Comprar aplicaciones o enviarnos un correo en la cuenta 
               #    luis@magro.com.mx para ayudarte"),
               h5("Si tienes alguna duda de como funciona esta app, puedes enviarnos un correo a 
                  luis@magro.com.mx para ayudarte. Vrs-2.0"),
               tabsetPanel(
                    tabPanel("Datos leidos",
                             DT::dataTableOutput("tabla_completa")),
                    tabPanel("Analisis de asignacion", 
                             #tableOutput("mejora"),
                             column(4,tableOutput("total.fam")),
                             column(12, p("Los siguientes graficos permiten entender la forma en que estan
                                          asignados los modelos a cada linea de produccion y revisar si la
                                          asignacion tiene errores, visualmente se puede verificar la 
                                          consistencia en la asignacion.")),
                             column(12,plotlyOutput("grafico.final", height = "1500px"))
                             ),
                    tabPanel("Analisis de personal",
                             column(2, h3("Total personas requeridas"),
                                    verbatimTextOutput("grantotal"), offset = 0),
                             column(4,h4("Personas por linea"), 
                                    tableOutput("Totales.por.linea")),
                             column(4, h4("Personas por puesto"),
                                    tableOutput("total_puesto")),
                             column(12,h5(" ")),
                             column(12,h4("Resumen por linea")),
                             DT::dataTableOutput("Porlinea",width = 600)
                             ),
                    tabPanel("Analisis de desviaciones",
                             column(4,uiOutput("seleccion_linea")),
                             column(8,sliderInput("quant", "Limite para considerar como desviacion",
                                                  min = 0, max = 49, step = 1, value = 10)),
                             column(3,p("Desviacion al cancelar criticos"),
                                    verbatimTextOutput("nva.desviacion"),
                                    p("Porcentaje de disminucion de desviacion"),
                                    verbatimTextOutput("pct.mejora"),
                                    p("Incremento de capacidad de produccion"),
                                    verbatimTextOutput("incr.produccion"),
                                    h3("Incremento de la facturacion semanal"),
                                    verbatimTextOutput("inc.facturacion")
                                    ),
                             column(3, p("Mayores causantes de desviacion"),
                                    tableOutput("cancelar.criticos")),
                             column(6, p("Indicadores de desviacion"),
                                    tableOutput("indicador.desviacion")),
                             checkboxInput("same.scale.fin", 
                                                    "Usar escala independiente en cada grafico", FALSE),
                             column(12,plotlyOutput("plot.por.linea", height = "800px")),
                             DT::dataTableOutput("desviaciones", width = 200)
                             ),
                    tabPanel("Analisis de flujo continuo",
                             column(4, uiOutput("flujo.deptos")),
                             column(2, uiOutput("flujo.estilo")),
                             column(4, checkboxInput("acumular", 
                                                     "Acumular estilos", FALSE)),
                             column(12, plotlyOutput("plot.flujo")),
                             column(12,DT::dataTableOutput("tabla_flujo", width = 600))
                             ),
                    tabPanel("Calculos",
                             h2("Definicion de calculos"),
                             h4("1. Se considera que todos los estilos son requieren del departamento
                                FAMILIA"),
                             h4("2. Se agregan todos los estilos existentes en FAMILIA a todos los 
                                departamentos en todas las
                                funciones con valor = 0 (de otra forma, las funciones no existentes en
                                estos departamentos no son tomadas en cuenta, cuando en realidad tienen 
                                un tiempo de proceso 0)"),
                             h4("3. Al convertir tiempo en personas, se consideran para fabricar 1000 pares
                                por dia"),
                             h4("4. El indicador de desviacion es la suma de la desviacion promedio de cada
                                funcion por linea"),
                             h4("5. El valor minimo y maximo es la suma de los maximos promedio de cada 
                                fucnion por linea"),
                             h4("6. El incremento en la capacidad de produccion es el porcentaje de disminucion
                                en el tiempo maximo de cada linea"),
                             h4("7. Incremento de facturacion semanal, incremento en capacidad de produccion
                                por pares producidos por 5 dias por precio promedio")
                             )
               )
          )
     )
))
