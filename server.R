#ANALISIS DE PRODUCCION

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)


shinyServer(function(input, output, session) {
     options(shiny.maxRequestSize=15*1024^2)
     
     #tabla vacia para cargar pares por linea
     tb.porproduc <- data.frame("Linea" = numeric(0), "Pares" = numeric(0))
     
     
     #lectura inicial de datos y transformacion al formato requerido de estilo-puesto1-puesto2
     lectura.inicial <- reactive({
          require(dplyr)
          require(tidyr)
          
          inFile <- NULL
          inFile <- input$browse          
          #inFile$datapath <- "tiempos.csv"
          
          if (is.null(inFile)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          #leer por tipo de archivo
          tiempos.raw <- read.csv(inFile$datapath, 
                            header = TRUE, na.strings = c("NA",""))
          
          #limpiar el formato, actual (LINEA, VCESTIL, PARES, FAMPESP, FAMMONT, DEPTO, FUNCION, TIEMPO,
          #PERSONAS, META)
          deptos.usar <- c("CORTE","CORTE Y PREPARA", "ENSAMBLES", "FAMILIA", "FORRADOS", 
                           "PLANTA", "RAYADO Y RESACA",
                           "SUELA")
          
          tiempos.raw <- tiempos.raw%>%
               select(DEPTO, VCESTIL, FAMPESP, FUNCION, TIEMPO)%>%
               filter(DEPTO %in% deptos.usar)%>%
               filter(TIEMPO > 0)
          
          
          #agrupar pespuntadores y preliminares
          tiempos.raw$FUNCION <- ifelse(grepl("PESPUNTADOR", 
                                              tiempos.raw$FUNCION),"PESPUNTADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-PES", 
                                              tiempos.raw$FUNCION),"PESPUNTADOR",
                                        paste(tiempos.raw$FUNCION))
          #corregir del sistema, mientras, debe desaparecer
          tiempos.raw$FUNCION <- ifelse(grepl("PRECONFORM", 
                                              tiempos.raw$FUNCION),"PRELIMINAR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("PRELIM", 
                                              tiempos.raw$FUNCION),"PRELIMINAR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-PRE", 
                                              tiempos.raw$FUNCION),"PRELIMINAR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("C-PREA", 
                                              tiempos.raw$FUNCION),"PRELIMINAR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-COR", 
                                              tiempos.raw$FUNCION),"CORTADOR PIEL",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CORTADOR FLASH", 
                                              tiempos.raw$FUNCION),"CORTADOR PIEL",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("PRECONF", 
                                              tiempos.raw$FUNCION),"PRECONFORMADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-PREC", 
                                              tiempos.raw$FUNCION),"PRECONFORMADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-DOB", 
                                              tiempos.raw$FUNCION),"DOBLILLADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-REB", 
                                              tiempos.raw$FUNCION),"REBAJADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("REBAJADOR PIEL", 
                                              tiempos.raw$FUNCION),"REBAJADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw[tiempos.raw$FUNCION == "FORRAR" & tiempos.raw$DEPTO == "PLANTA",]$FUNCION <- "RIBETEAR"
          
          #quitar comodines
          tiempos.raw <- tiempos.raw[!grepl("COMODIN+",  tiempos.raw$FUNCION),]
          names(tiempos.raw) <- make.names(names(tiempos.raw))
          tiempos <- tiempos.raw%>%
               select(DEPTO, "ESTILO" = VCESTIL, "LINEA" = FAMPESP, FUNCION, TIEMPO)
          
          
          #hacer correcciones al archivo y llenar combo para seleccionar depto
          if (is.null(tiempos)){
               #sin archivo seleccionado
               return(NULL)
          }
          
 
          #llenar combo de departamentos con el archivo preparado
          output$depto.select <- renderUI({
               deptos <- unique(tiempos$DEPTO)
               selectInput("depto.selected", "Selecciona el departamento que quieres analizar", as.list(deptos))
          })
          

          #llenar combo de lineas de produccion con el archivo preparado
          output$fams.select <- renderUI({
               fams <- unique(tiempos$LINEA)%>%sort()
               selectInput("fams.selected", "Selecciona las lineas que quieres analizar", as.list(fams), multiple = TRUE)
          })
          
          
          return(tiempos)
     })
          
 
          
     #Datos leidos - tabla de estilos y tiempos
     output$tabla_completa <- DT::renderDataTable({
          tabla.raw <- reporte.final()
          if(is.null(tabla.raw)) return(NULL)
          
          DT::datatable(tabla.raw, options = list(pageLength = 25))
     })
     
     
     #Menu - filtra deptos y lineas, completa con cero las funciones que no aparecen en los deptos
     #esto es logico, pues no se cargan los puestos que no se utilizan, pero si causan desviacion
     #pues realmente se tiene gente y no se utiliza.  NO CORRIGE AL SELEECIONAR MOSTRAR COMO PERSONAS
     
     reporte.final.sin.personas <- reactive({
          datos <- lectura.inicial()
          if (is.null(datos)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          #leer combo y definir lineas a usar, sirve para quitar estilos sin linea asignada
          #y para quitar canteras o maquilas, dan problemas al agrupar en un solo depto pues
          # en realidad es no se procesa junto
          if (is.null(input$fams.selected)) return(NULL)
          
          datos <- datos%>%
               filter(LINEA %in% input$fams.selected)
          
          #completar con cero las funciones que existen en cada depto y el estilo no las tiene
          #primero se debe agregar todos los estilos a todas los deptos
          estilos.fam <- unique(datos[datos$DEPTO == "FAMILIA",2:3])
          #estilos.fam <<- unique(datos[datos$DEPTO == "FAMILIA",2:3])
          
          #verificar que esa linea tenga ese departamento
          if (nrow(datos[datos$DEPTO== input$depto.selected,]) == 0) return(NULL)
          
          #hacer esto cuando se seleccione el depto
          temp <- datos%>%
               filter(DEPTO == input$depto.selected)%>%
               select(ESTILO, FUNCION, TIEMPO)%>%
               group_by(ESTILO, FUNCION)%>%
               summarise("TIEMPO" = sum(TIEMPO))%>%
               spread(FUNCION,TIEMPO, drop = FALSE, fill = 0)
          datos <- merge(estilos.fam, temp, by = "ESTILO")
          
          
          #agrupar en una sola linea
          #cambia los datos de la columna linea todo a 1
          if(input$agrupado){
               datos[,2] <- 1
          }
          
          #convertir NAS en cero
          datos[is.na(datos)]<-0

          #estilo y linea como factor
          datos[,1] <- as.factor(datos[,1])
          datos[,2] <- as.factor(datos[,2])
          #nas a cero
          datos[is.na(datos)]<-0
          
          #limitar cantidad de modelos
          #datos<- datos[1:20,]
          
          
          return(datos)
          
     })  

     #Menu - filtra deptos y lineas, completa con cero las funciones que no aparecen en los deptos
     #esto es logico, pues no se cargan los puestos que no se utilizan, pero si causan desviacion
     #pues realmente se tiene gente y no se utiliza.
     #CALCULA PERSONAS EN LUGAR DE TIEMPOS, BASADO EN 100 PARES POR HORA
     
     reporte.final <- reactive({
          datos <- lectura.inicial()
          if (is.null(datos)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          #leer combo y definir lineas a usar, sirve para quitar estilos sin linea asignada
          #y para quitar canteras o maquilas, dan problemas al agrupar en un solo depto pues
          # en realidad es no se procesa junto
          if (is.null(input$fams.selected)) return(NULL)
          
          
          datos <- datos%>%
               filter(LINEA %in% input$fams.selected)
          
          #convertir los tiempos en personas utilizando sliders (para 1000 pares por dia)
          if (input$personas) {
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               datos <-  datos%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*1000/(efic*hrs*3600)))%>%
                    select(DEPTO, ESTILO, LINEA, FUNCION, "TIEMPO" = PERSONAS)
          }
          
          #completar con cero las funciones que existen en cada depto y el estilo no las tiene
          #primero se debe agregar todos los estilos a todas los deptos
          estilos.fam <- unique(datos[datos$DEPTO == "FAMILIA",2:3])
          #estilos.fam <<- unique(datos[datos$DEPTO == "FAMILIA",2:3])
          
          #verificar que esa linea tenga ese departamento
          if (nrow(datos[datos$DEPTO== input$depto.selected,]) == 0) return(NULL)
          
          #hacer esto cuando se seleccione el depto
          temp <- datos%>%
               filter(DEPTO == input$depto.selected)%>%
               select(ESTILO, FUNCION, TIEMPO)%>%
               group_by(ESTILO, FUNCION)%>%
               summarise("TIEMPO" = sum(TIEMPO))%>%
               spread(FUNCION,TIEMPO, drop = FALSE, fill = 0)
          datos <- merge(estilos.fam, temp, by = "ESTILO")
          
          
          #agrupar en una sola linea
          #cambia los datos de la columna linea todo a 1
          if(input$agrupado){
               datos[,2] <- 1
          }
          
          #convertir NAS en cero
          datos[is.na(datos)]<-0
          
          #estilo y linea como factor
          datos[,1] <- as.factor(datos[,1])
          datos[,2] <- as.factor(datos[,2])
          #nas a cero
          datos[is.na(datos)]<-0
          
          #limitar cantidad de modelos
          #datos<- datos[1:20,]
          
          
          return(datos)
          
     })  
     
     # General - Drop-down de linea a definir pares por producir
     output$lineas.selected <- renderUI({
          num.fam <- reporte.final()
          if(is.null(num.fam)) return(NULL)
          
          l.linea <- unique(num.fam$LINEA)%>%sort()
          selectInput("linea.seleccionada", "Linea de produccion", as.list(l.linea))
     })
     
     #General - crea la tabla de pares por producir por linea
     actualiza <- eventReactive(input$agregar, {
          #agregar a la tabla los pares
          l.linea <- input$linea.seleccionada
          pares <- as.numeric(input$pares)
          
          temp <- data.frame("LINEA" = l.linea, "PARES" = pares)
          
          #quita la linea que existe (actualizar)
          tb.porproduc <- tb.porproduc%>%filter(LINEA != l.linea)
          tb.porproduc <<- rbind(tb.porproduc, temp)%>%
               arrange(LINEA)
     })
     
     #General - imprime tabla de pares por producir
     output$por.producir <- DT::renderDataTable({

          tabla <- actualiza()               
          DT::datatable(tabla, options = list(dom = 't'))
     })
     
     
     # General - Drop-down de linea a filtrar
     output$seleccion_linea <- renderUI({
          num.fam <- reporte.final()
          if(is.null(num.fam)) return(NULL)
          l.linea <- unique(num.fam$LINEA)%>%sort()
          selectInput("dataset", "Filtrar por linea de produccion", as.list(l.linea))
     })
     
     #Analisis de asignacion - Grafico por puesto
     output$grafico.final <- renderPlotly({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {

               #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
               fin <- dim(reporte)[2]
               tabla.renglon <- gather(reporte, "PUESTO","TIEMPO",c(3:fin))
               
               unidades <- ifelse(input$personas, "Personas", "Segundos")
               
               # grafico de desviaciones por puesto
               ggplotly(
                    ggplot(data = tabla.renglon, aes(ESTILO, TIEMPO, colour = LINEA)) + 
                         geom_point() + 
                         facet_grid(PUESTO~., as.table = F, scales = "free") +
                         xlab("Estilos") +
                         ylab(unidades)  +
                         ggtitle("Dispersion de tiempo/personas para producir un par") +
                         theme(axis.text=element_text(size=8))
               )
          } 
          
          
     })
     
     #Desviaciones - escala independiente por grafico
     free.scale.fin <- reactive({
          b.scales = "fixed"
          if (input$same.scale.fin){
               b.scales = "free"
          }
          return(b.scales)
          
     })
     

     #Desviaciones - grafico por linea, dependen del combobox cual mostrar
     obtener.criticos <- reactive({
          reporte <- reporte.final()
          if(is.null(reporte)) return(NULL)
          
          limites <- input$quant
          
          linea <- input$dataset
          cols.usar <- c(3:(ncol(reporte)))
          f.plot <- reporte%>%filter(LINEA == linea)%>%
               gather("PUESTO","TIEMPO",cols.usar)
          
          
          #10% inferior y superior
          quant <- f.plot%>%
               group_by(PUESTO)%>%
               summarise("Promedio" = mean(TIEMPO),
                         "Q1" = quantile(TIEMPO, probs = limites/100), 
                         "Q4" = quantile(TIEMPO, probs = 1-(limites/100)))
          
          temp <- merge(f.plot, quant, by = "PUESTO")%>%
               mutate("Distancia" = sqrt((Promedio-TIEMPO)^2))
          
          #distancia maxima, tomando en cuanta todas las fracciones (critico)     
          critico <- temp%>%
               group_by(ESTILO)%>%
               summarise("Distancia" = sum(Distancia))%>%
               mutate("Q1" = quantile(Distancia, probs = limites/100), 
                      "Q4" = quantile(Distancia, probs = 1-(limites/100)),
                      "CRITICO" = ifelse(Distancia > Q4, "CRITICO", "NORMAL"))%>%
               select(ESTILO, CRITICO)
          
          for.plot <- merge(temp, critico, by = "ESTILO")%>%
                      mutate("DESVIACION" = ifelse(CRITICO == "CRITICO", "CRITICO",
                                                   ifelse(TIEMPO < Q1 | TIEMPO > Q4,"FUERA","NORMAL")))
     })
     
     #Desviaciones - Imprimir los estilos criticos segun el slider de porcentaje en los limites
     output$cancelar.criticos  <- renderTable({
          for.plot <- obtener.criticos()
          
          if (is.null(for.plot)) return(NULL)
          
          temp <- for.plot%>%
               filter(DESVIACION == "CRITICO")%>%
               select(ESTILO, LINEA)
          
          criticos <- unique(temp) 
          
          return(criticos)
          
     })
     
     #Desviaciones - Grafico por linea, dependen del combobox cual mostrar
     output$plot.por.linea <- renderPlotly({
          for.plot <- obtener.criticos()
          
          if (is.null(for.plot)) return(NULL)
          
          b.scales <- free.scale.fin()
          
          #cuando se cambian tiempos por personas
          unidades <- ifelse(input$personas, "Personas", "Segundos")
          
          ggplotly(
          ggplot(for.plot, aes(ESTILO,TIEMPO, colour = DESVIACION)) +
               geom_point() +
               facet_wrap(~PUESTO, ncol=2, strip.position = "right" ,scales = b.scales, as.table = T) +
               theme(strip.background = element_blank(), strip.placement = "outside") + 
               geom_hline(data = for.plot%>%
                               group_by(PUESTO)%>%
                               summarise("Promedio" = mean(TIEMPO)),
                          aes(yintercept = Promedio), col = "red", lwd = 0.3) + 
               geom_hline(data = for.plot%>%
                               group_by(PUESTO)%>%
                               summarise("Promedio.real" = ceiling(mean(TIEMPO))),
                          aes(yintercept = Promedio.real), col = "navy", lwd = 0.5) +
               ylab(unidades) + 
               ggtitle("Estilos a producir vs tiempo total de proceso por funcion")
          )
     })
     
     #Desviaciones - Imprimir desviaciones por linea
     output$total.fam <- renderTable({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
               totales <- reporte%>%
                    group_by(LINEA)%>%
                    summarise("Estilos por familia"= n())
          }
     })
     
     #Desviaciones - indicador general de desviacion (imprimir)
     output$indicador.desviacion <- renderTable({
          tabla <- fun.indicador.desviacion()
          if (is.null(tabla)) return(NULL)
          temp <- tabla
          return(temp)
     })
     
     #Desviaciones - calcula tabla de desviaciones
     fun.indicador.desviacion <- reactive({
          reporte <- reporte.final()
          if(is.null(reporte)) return(NULL)
          
               #indicador de desviacion (promedio/(max-min))
               #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
               fin <- dim(reporte)[2]
               tabla.renglon <- gather(reporte, "PUESTO","TIEMPO",c(3:fin))
               
               indicador <- tabla.renglon%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("Prom" = ceiling(mean(TIEMPO)),
                              "Mini" = min(TIEMPO) ,
                              "Maxi" = max(TIEMPO))%>%
                    group_by(LINEA)%>%
                    summarise("Promedio" = sum(Prom),
                              "Minimo" = sum(Mini),
                              "Maximo" = sum(Maxi),
                              "Porcentaje de desviacion" = round((Maximo-Minimo)/Promedio*100,2))
               return(indicador)

     })
     
     #Desviaciones - Imprimir valor de porcentaje de mejora
     output$pct.mejora <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          anterior <- fun.indicador.desviacion()
          if(is.null(anterior)) return(NULL)
          
          ant <- anterior%>%
               filter(LINEA == input$dataset)
          
          cat(as.numeric(100-round(mejora$NvaDesviacion/ant[5]*100,2)))
     
     })
     
     #Desviaciones - Imprimir valor de desviacion mejorada
     output$nva.desviacion <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          cat(as.numeric(mejora$NvaDesviacion))
     })
     
     #Desviaciones - Imprimir dato de aumento de produccion
     output$incr.produccion <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          anterior <- fun.indicador.desviacion()
          if(is.null(anterior)) return(NULL)
          
          incr <- round((1-(mejora$Maximo/anterior%>%
                                 filter(LINEA == input$dataset)%>%
                                 select(Maximo)))*100,2)
          
          cat(as.numeric(incr))
     })

     
     #Desviaciones - indicador general de desviacion sin criticos
     desviacion.mejorada <- reactive({
          
          reporte <- obtener.criticos()
          if(is.null(reporte)) return(NULL)

               mejora <- reporte%>%
                    filter(CRITICO != "CRITICO")%>%
                    select(ESTILO, LINEA, PUESTO, TIEMPO)%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("Prom" = ceiling(mean(TIEMPO)),
                              "Mini" = min(TIEMPO) ,
                              "Maxi" = max(TIEMPO))%>%
                    group_by(LINEA)%>%
                    summarise("Promedio" = sum(Prom),
                              "Minimo" = sum(Mini),
                              "Maximo" = sum(Maxi),
                              "NvaDesviacion" = round((Maximo-Minimo)/Promedio*100,2))
               return(mejora)
     })
     
     #Desviaciones - tabla de desviaciones por linea
     output$desviaciones <- DT::renderDataTable({
          reporte <- reporte.final()
          if(is.null(reporte)) return(NULL)
          
               #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
               fin <- dim(reporte)[2]
               tabla.renglon <- gather(reporte, "PUESTO","TIEMPO",c(3:fin))
               
               desviaciones <- tabla.renglon%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("Promedio" = ceiling(mean(TIEMPO)),
                              "Desviacion" = round(sd(TIEMPO),2),
                              "Minimo" = min(TIEMPO) ,
                              "Maximo" = max(TIEMPO))
               DT::datatable(desviaciones, options = list(pageLength = 50))

     })
     
     
     #Flujo continuo -  Drop-down de departamentos, para seleccionar para flujos
     output$flujo.deptos <- renderUI({
          deptos <- lectura.inicial()
          if(is.null(deptos)) return(NULL)
          un.deptos <- unique(deptos$DEPTO)%>%sort()
          selectInput("cb.deptos.flujo", "Selecciona departamentos para revisar", as.list(un.deptos),
                      multiple = T)
     })
     
     #Flujo continuo -  Drop-down linea para revisar flujo
     output$flujo.linea <- renderUI({
          datos <- reporte.final.sin.personas()
          if(is.null(datos)) return(NULL)
          
          lineas <- unique(datos$LINEA)%>%sort()
          selectInput("cb.lineas.flujo", "Selecciona una linea", as.list(lineas),
                      multiple = F)
     })
     
     #Flujo continuo -  Drop-down de estilos para flujo
     output$flujo.estilo <- renderUI({
          datos <- reporte.final.sin.personas()
          if(is.null(datos)) return(NULL)
          
               datos <- datos%>%filter(LINEA == input$cb.lineas.flujo)
               estilos <- unique(datos$ESTILO)%>%sort()
               selectInput("cb.estilos.flujo", "Selecciona un estilo", as.list(estilos),
                           multiple = T)
     })
     
     #Flujo continuo - calcula plantilla, completa las funciones con cero (basado en reporte final)
     reporte.flujo <- reactive({
          datos <- lectura.inicial()
          if (is.null(datos)) return(NULL)
          
          #leer combo y definir lineas a usar, sirve para quitar estilos sin linea asignada
          #y para quitar canteras o maquilas, dan problemas al agrupar en un solo depto pues
          # en realidad es no se procesa junto
          if (is.null(input$fams.selected)) return(NULL)
          
          #si agrupado, convertir todo a linea 1
          #cambia los datos de la columna linea todo a 1
          if(input$agrupado){
               datos[,3] <- 1
          }
          
          #convertir los tiempos en personas para 1000 pares al dia
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               datos <-  datos%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*1000/(efic*hrs*3600)))%>%
                    select(DEPTO, ESTILO, LINEA, FUNCION, PERSONAS)
          
          #lineas seleccionadas
          datos <- datos%>%
               filter(LINEA %in% input$cb.lineas.flujo)

          
          #completar con cero las funciones que existen en cada depto y el estilo no las tiene
          #primero se debe agregar todos los estilos a todas los deptos
          estilos.fam <- unique(datos[datos$DEPTO == "FAMILIA",2:3])
          #estilos.fam <<- unique(datos[datos$DEPTO == "FAMILIA",2:3])

          #crear bd vacia
          bd <- data.frame("ESTILO" = numeric(0),
                           "LINEA" = numeric(0),
                           "FUNCION"= numeric(0),
                           "PERSONAS" = numeric(0),
                           "DEPTO" = numeric(0))
          tiempos <- datos
          
          #calcula y completa con cero el personal de cada estilo que no aparece
          for (i in input$cb.deptos.flujo){
               #se pueden seleccionar varios deptos - es lo mas logico
               temp <- tiempos%>%
                    filter(DEPTO %in% i)%>%
                    select(DEPTO, ESTILO, FUNCION, PERSONAS)%>%
                    group_by(ESTILO, FUNCION)%>%
                    summarise("PERSONAS" = sum(PERSONAS))%>%
                    spread(FUNCION,PERSONAS, drop = FALSE, fill = 0)
               datos <- merge(estilos.fam, temp, by = "ESTILO")
               
               
               #agrupar en una sola linea
               #cambia los datos de la columna linea todo a 1
               if(input$agrupado){
                    datos[,2] <- 1
               }
               
               #convertir NAS en cero
               datos[is.na(datos)]<-0
               
               #estilo y linea como factor
               datos[,1] <- as.factor(datos[,1])
               datos[,2] <- as.factor(datos[,2])
               #nas a cero
               datos[is.na(datos)] <- 0
               
               #hacer formato trabajable
               datos <- gather(datos, FUNCION, PERSONAS, 3:ncol(datos))
               
               #agregar el departamento
               datos$DEPTO <- i
               
               #bd completa, todos los deptos, y funciones completas para cada depto
               bd <- rbind(bd, datos)
               
          }
          
          #plantilla basica por promedio
          plantilla <- bd%>%
               group_by(LINEA, DEPTO, FUNCION)%>%
               summarise("PLANTILLA" = ceiling(mean(PERSONAS)))
          
          #diferencias por estilo vs plantilla
          completa <- merge(bd, plantilla , by = c("LINEA","DEPTO","FUNCION"))
          
          return(completa)
          
          
     })  
     
     
     #Flujo continuo - calcula plantilla basica, metas y cumplimiento por depto, linea
     full.flujo <- reactive({
          
               datos <- reporte.flujo()
               if(is.null(datos)) return(NULL)
               
               estilos = input$cb.estilos.flujo
               #cuantos estilos seleccionados
               cuantos <- length(input$cb.estilos.flujo)               
               if (cuantos == 0) return(NULL)
               if (length(input$cb.lineas.flujo) == 0) return(NULL)
               
               #solo los estilos seleccionados
               datos <- datos%>%
                    filter(ESTILO %in% estilos)
               
               #si no hay pares por producir por linea utilizar 100 pares por hora
               #if(nrow(tb.porproduc)==0) return(NULL)
               
               #acumular meta de los estilos seleccionados si es mas de uno
               if (cuantos > 1){
                    tabla.plot <- datos%>%
                         group_by(DEPTO, FUNCION)%>%
                         summarise("PERSONAS" = ceiling(sum(PERSONAS)/cuantos),
                                   "PLANTILLA" = min(sum(PLANTILLA)/cuantos),
                                   "Pct.meta" = ifelse(PERSONAS == 0, 
                                                       ceiling(300), 
                                                       ceiling((PLANTILLA/(PERSONAS))*100)))%>%
                         mutate("ESTILO" = "AGRUPADO")
               } else {
                    
                    tabla.plot <- datos%>%
                         mutate("Pct.meta" = ifelse(PERSONAS == 0, 
                                                    300, 
                                                    ceiling((PLANTILLA/PERSONAS)*100)))
               }
               
               plot.final <- tabla.plot%>%
                    arrange(DEPTO, FUNCION, Pct.meta)%>%
                    mutate("DEPTOFUNC" = paste(DEPTO,"/",FUNCION),
                           "EFIC" = round(min(Pct.meta)/Pct.meta,2),
                           "DIF" = PERSONAS - PLANTILLA)
               
               return(plot.final)
               
     })

     #Flujo continuo - tabla del grafico (pendiente si presentar o no)
     output$tabla.plot <- DT::renderDataTable({
          plot.final <- full.flujo()

          if (is.null(plot.final))  return(NULL)
          
          plot.final <- plot.final%>%
               select(ESTILO, DEPTO, FUNCION, PERSONAS, PLANTILLA, "PCT.META" = Pct.meta, EFIC)
          
          DT::datatable(plot.final, options = list(pageLength = 25))
     })
     
     #Flujo continuo - tabla completa de flujo (pendiente si presentar o no)
     output$tabla.full <- DT::renderDataTable({
          plot.final <- reporte.flujo()
          
          if (is.null(plot.final))  return(NULL)
          DT::datatable(plot.final, options = list(pageLength = 25))
     })
     
     #Flujo continuo - tabla completa de flujo balanceado (pendiente si presentar o no)
     output$balanceo <- DT::renderDataTable({
          balanceado <- balanceo()
          if (is.null(balanceado)) return(NULL)
          
          balanceado <- balanceado%>%
               select(ESTILO, DEPTO, FUNCION, PERSONAS, PLANTILLA, "PCT.META" = Pct.meta, EFIC)
          
          DT::datatable(balanceado, options = list(pageLength = 25))
     })
     
     #Flujo continuo - realiza el balanceo para mejorar metas y eficiencia
     balanceo <- reactive({
          
          plot.final <- full.flujo()
          if (is.null(plot.final))  return(NULL)
          #cuantos estilos seleccionados
          if (length(input$cb.estilos.flujo) == 0) return(NULL)
          if (length(input$cb.lineas.flujo) == 0) return(NULL)
          
          
          cumplimiento = min(plot.final$Pct.meta)
          eficiencia = mean(plot.final$EFIC)
          if (is.infinite(cumplimiento)) return(NULL)
          
          #buscar cumplimineto y eficiencia mayor al 90 o 10 iteraciones
          iter = 0
          
          movimientos <- data.frame("Origen" = numeric(0),"Destino" = numeric(0))

          while((cumplimiento < 90 | eficiencia < .9) & iter < 5){
               dona <- min(plot.final$DIF)
               recibe <- min(plot.final$Pct.meta)
               
               movimientos <- c(movimientos, data.frame("Origen" = dona, "Destino" = recibe))
               #movimientos <<- c(movimientos, data.frame("Origen" = dona, "Destino" = recibe))
               
               #pueden haber 2 que donan y 2 que reciben, se elige el primero
               funcionrecibe <- head(plot.final[plot.final$Pct.meta == recibe,]$FUNCION,1)
               funciondona <- head(plot.final[plot.final$DIF == dona,]$FUNCION,1)
               
               if (dona >= 0) break
     
               #en el minimo, quien recibe +1, quien dona -1
               nva.plantilla.mas <- plot.final[plot.final$FUNCION == funcionrecibe,]$PLANTILLA + 1
               nva.plantilla.menos <- plot.final[plot.final$FUNCION == funciondona,]$PLANTILLA - 1
               
               #cuantas personas quiere el estilo en esos dos, donador y receptor
               personas.mas <- plot.final[plot.final$FUNCION == funcionrecibe,]$PERSONAS
               personas.menos <- plot.final[plot.final$FUNCION == funciondona,]$PERSONAS
               
               #nuevas metas
               nva.meta.mas <- round((nva.plantilla.mas /personas.mas)*100,2)
               nva.meta.menos <- ifelse(personas.menos ==0,300, round((nva.plantilla.menos /personas.menos)*100,2))
               
               #si ahora min meta es mayor (aun cuando le quitamos a una persona, actualiza)
               if(recibe < nva.meta.menos | nva.meta.menos %in% Inf){
                    nva.plantilla.mas -> plot.final[plot.final$FUNCION == funcionrecibe,]$PLANTILLA
                    nva.plantilla.menos -> plot.final[plot.final$FUNCION == funciondona,]$PLANTILLA
                    
                    plot.final <- plot.final%>%
                         mutate("Pct.meta" = ifelse(PERSONAS == 0, 
                                                    300, 
                                                    ceiling((PLANTILLA/PERSONAS)*100)))%>%
                         arrange(DEPTO, FUNCION, Pct.meta)%>%
                         mutate("DEPTOFUNC" = paste(DEPTO,"/",FUNCION),
                                "EFIC" = round(min(Pct.meta)/Pct.meta,2),
                                "DIF" = PERSONAS - PLANTILLA)
                    
               } else { break }
               
               #actualiza para el ciclo
               cumplimiento = min(plot.final$Pct.meta)
               eficiencia = mean(plot.final$EFIC)
               
               dona <- min(plot.final$DIF)
               recibe <- min(plot.final$Pct.meta)
               
               iter=iter+1
          }
          
          return(plot.final)
     })
     
     
     #Flujo continuo - Imprime cumplimiento de meta inicial
     output$cumpl.meta <- renderPrint({
          plot.final <- full.flujo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(min(plot.final$Pct.meta))),"%")
     })
     
     #Flujo continuo - Imprime aumento en pares producidos al balancear
     output$aumento.pares <- renderPrint({
          inicial <- full.flujo()
          balanceado <- balanceo()
          
          if (is.null(inicial))  return(NULL)
          if (is.null(balanceado))  return(NULL)
          
          mas.pares <- (((min(balanceado$Pct.meta))/ceiling(min(inicial$Pct.meta)))-1)*100*input$horas.trabajo*5
          
          cat(format(ceiling(mas.pares), decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
     })
     
     #Flujo continuo - Imprime aumento en facturacion al balancear
     output$aumento.facturacion <- renderPrint({
          inicial <- full.flujo()
          balanceado <- balanceo()
          
          if (is.null(inicial))  return(NULL)
          if (is.null(balanceado))  return(NULL)
          
          mas.pares <- (((min(balanceado$Pct.meta))/ceiling(min(inicial$Pct.meta)))-1)*100*input$horas.trabajo*5
          
          cat(format(ceiling(mas.pares)*input$precio.prom, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
     })
     
     #Flujo continuo - Imprime cumplimiento mejorado al balancear
     output$cumpl.mejorado <- renderPrint({
          plot.final <- balanceo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(min(plot.final$Pct.meta))),"%")
     })
     
     #Flujo continuo - Imprime eficiencia esperada inicial
     output$ef.esperada <- renderPrint({
          plot.final <- full.flujo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(mean(plot.final$EFIC)*100),"%"))
     })
     
     #Flujo continuo - Imprime eficiencia mejorada con balanceo
     output$ef.mejorada <- renderPrint({
          plot.final <- balanceo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(mean(plot.final$EFIC)*100),"%"))
     })
     
     #Flujo continuo - tabla de movimientos requeridos con balanceo
     output$tabla.movimientos <- renderTable({
          datos.normal <- full.flujo()
          if (is.null(datos.normal))  return(NULL)
          datos.normal <- datos.normal%>%mutate("Datos" = "Base")
          datos.normal$PLANTILLA <- datos.normal$PLANTILLA * -1
          
          balanceado <- balanceo()
          balanceado <- balanceado%>%mutate("Datos" = "Balanceado")
          
          
          #junta las tablas
          result <- rbind(datos.normal, balanceado)%>%
               group_by(FUNCION)%>%
               summarise("CANTIDAD" = sum(PLANTILLA))%>%
               filter(CANTIDAD != 0)
          
          return(result)
          
     })
     
     #Flujo continuo - Observa cuando se seleccionan estilos
     observeEvent(input$cb.estilos.flujo, {
          
          #Flujo continuo - Imprime grafico de flujo continuo
          output$plot.flujo <- renderPlotly({
               
               datos.normal <- full.flujo()
               if (is.null(datos.normal))  return(NULL)
               datos.normal <- datos.normal%>%mutate("Datos" = "Base")
               
               balanceado <- balanceo()
               balanceado <- balanceado%>%mutate("Datos" = "Balanceado")
               
               #META BASE
               intercepts <- datos.normal%>%
                    group_by(ESTILO, DEPTO)%>%
                    filter(Pct.meta > 0)%>%
                    summarise("Meta.real" = min(Pct.meta))
               
               #META BALANCEO
               intercepts.b <- balanceado%>%
                    group_by(ESTILO, DEPTO)%>%
                    filter(Pct.meta > 0)%>%
                    summarise("Meta.real" = min(Pct.meta))
               
               para.plot <- rbind(datos.normal, balanceado)
               
               ggplot(para.plot, aes(DEPTOFUNC, Pct.meta, colour = Datos, group = Datos)) + 
                    geom_point(size = 2) + geom_line() + 
                    scale_x_discrete(labels = substr(para.plot$FUNCION,1,3)) +
                    geom_hline(data = intercepts, aes(yintercept =  Meta.real, colour = DEPTO))+
                    geom_hline(data = intercepts.b, aes(yintercept =  Meta.real, colour = DEPTO))+
                    expand_limits(y = c(0,100)) + 
                    ggtitle("Porcentaje de cumplimiento de meta por funcion")
               
          })
          
     })
     
     
     #General - Oberva boton agregar y actualiza todo lo referente a analisis de personal
     #cuando se carga un meta de pares por linea
     observeEvent(input$agregar, {
          
          #Analisis de desviaciones - incremento en facturacion quitando criticos
          output$inc.facturacion <- renderPrint({
               mejora <- desviacion.mejorada()
               if(is.null(mejora)) return(NULL)
               
               #si no hay pares por producir por linea no hace el calculo de personas
               if(nrow(tb.porproduc)==0) return(NULL)               
               
               anterior <- fun.indicador.desviacion()
               if(is.null(anterior)) return(NULL)
               
               incr <- round((1-(mejora$Maximo/anterior%>%
                                      filter(LINEA == input$dataset)%>%
                                      select(Maximo)))*100,2)

               l.actual <- input$dataset
               par.fam <- tb.porproduc%>%
                    filter(LINEA == l.actual)%>%
                    select(PARES)
               
               if (nrow(par.fam)==0) return(cat("Sin pares por producir"))
               
               mas.fact <- as.numeric(ceiling((par.fam * (incr/100) * as.numeric(input$precio.prom))*5))

               cat(format(mas.fact, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
          })
          
          #Analisis de personal - genera plantilla basica por linea, puesto, base de las demas
          calcular.plantilla <- reactive({
               temp <- reporte.final.sin.personas()
               if(is.null(temp)) return(NULL)
               #si no hay pares por producir por linea no hace el calculo
               if(nrow(tb.porproduc)==0) return(NULL)
               
               fin <- dim(temp)[2]
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               sds <- input$sds
               

               tabla.renglon <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
                    merge(tb.porproduc, by = "LINEA")%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)))%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("TIEMPO" = ceiling(mean(TIEMPO)),
                              "PARES" = min(PARES),
                              "PERSONAS" = ceiling(mean(PERSONAS+(sds*sd(PERSONAS)))))

               
               return(tabla.renglon)
               
          })
          
          #Analisis de personal - Tabla por linea-puesto
          output$PersonalPorlinea <- DT::renderDataTable({
               tabla.renglon <- calcular.plantilla()
               if (is.null(tabla.renglon)) return(NULL)
          
               DT::datatable(tabla.renglon, options = list(pageLength = 50))
               
          })
          

          #Analisis de personal - plantilla basica, Calcula restriccion y eficiencia por funcion
          eficiencia.funcion <- reactive({
               temp <- reporte.final.sin.personas()
               if(is.null(temp)) return(NULL)
               #si no hay pares por producir por linea no hace el calculo
               if(nrow(tb.porproduc)==0) return(NULL)
               
               fin <- dim(temp)[2]
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               sds <- input$sds
          
          #primero convierte a personas y luego redondea
          plantilla <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
               merge(tb.porproduc, by = "LINEA")%>%
               mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)))%>%
               group_by(LINEA, PUESTO)%>%
               summarise("TIEMPO" = ceiling(mean(TIEMPO)),
                         "PARES" = min(PARES),
                         "PLANTILLA" = ceiling(mean(PERSONAS+(sds*sd(PERSONAS)))))%>%
               select(LINEA, PUESTO, PARES, PLANTILLA)

  
               
               tabla.renglon <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
                    merge(plantilla, by = c("LINEA","PUESTO"))%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)),
                           "META" = ceiling(PLANTILLA/PERSONAS*100),
                           "PARES.PRODUCCION" = ceiling(PARES*META/100),
                           "CAPACIDAD.FUNCION" = ifelse(PARES<PARES.PRODUCCION, PARES, PARES.PRODUCCION))
               
               meta.estilo <- tabla.renglon%>%
                    group_by(ESTILO)%>%
                    summarise("PROD.RESTRICCION" = min(CAPACIDAD.FUNCION))
                    
               result <- merge(tabla.renglon, meta.estilo, by = "ESTILO")%>%
                    mutate("EFICIENCIA" =ceiling(PROD.RESTRICCION/PARES.PRODUCCION*100),
                           "APROVECHAMIENTO" = round(EFICIENCIA/100*PLANTILLA,2)) 
               

               return(result)
          })
          
          
          #Analisis de personal - Personal por linea con eficiencias
          output$PersonalPorEstilo <- DT::renderDataTable({
               result <- eficiencia.funcion()
               if (is.null(result)) return(NULL)
               
               DT::datatable(result, options = list(pageLength = 50))
               
          })
          
          #Analisis de personal - eficiencia por estilo (para graficar)
          output$eficiencia.estilo <- renderPlotly({
               temp <- eficiencia.funcion()
               if(is.null(temp)) return(NULL)
               
               eficiencia.por.estilo <- temp%>%
                    group_by(ESTILO)%>%
                    summarise("DISPONIBLE" = sum(PLANTILLA),
                              "UTILIZADO" = sum(APROVECHAMIENTO))%>%
                    mutate("EFICIENCIA" = ceiling(UTILIZADO/DISPONIBLE*100),
                           "NIVEL" = ifelse(EFICIENCIA > 85, "A-BUENO", ifelse(EFICIENCIA > 50, "B-REGULAR","C-CRITICO")))
               
               paleta <- c("darkgreen","gold2","red")
               
               ggplotly(
                    ggplot(data = eficiencia.por.estilo, aes(ESTILO, EFICIENCIA, colour = NIVEL)) +
                         geom_point() + 
                         scale_color_manual(values = paleta)+ 
                         expand_limits(y=c(0,100))
                    
               )
          })
          
          
          #Analisis de personal - eficiencia por estilo (para graficar)
          output$eficiencia.linea <- DT::renderDataTable({
               temp <- eficiencia.funcion()
               if(is.null(temp)) return(NULL)
               
               eficiencia.por.linea <- temp%>%
                    group_by(LINEA, ESTILO)%>%
                    summarise("DISPONIBLE" = sum(PLANTILLA),
                              "UTILIZADO" = sum(APROVECHAMIENTO))%>%
                    mutate("TEMP" = ceiling(UTILIZADO/DISPONIBLE*100))%>%
                    group_by(LINEA)%>%
                    summarise("PROMEDIO" = ceiling(mean(TEMP)),
                              "MEDIANA" = ceiling(quantile(TEMP, probs = 0.5)),
                              "DESVIACION" = round(sd(TEMP),2))
               
               DT::datatable(eficiencia.por.linea, options = list(pageLength = 10))
          })
          
          #Analisis de personal - eficiencia por estilo (para graficar)
          output$meta.linea <- DT::renderDataTable({
               temp <- eficiencia.funcion()
               if(is.null(temp)) return(NULL)
               
               meta.por.linea <- temp%>%
                    group_by(LINEA)%>%
                    summarise("PROMEDIO" = ceiling(mean(PROD.RESTRICCION)),
                              "MEDIANA" = ceiling(quantile(PROD.RESTRICCION, probs = 0.5)),
                              "DESVIACION" = round(sd(PROD.RESTRICCION),2),
                              "PAR.PRESUP" = min(PARES))%>%
                    mutate("CUMPLIMIENTO" = ceiling(PROMEDIO/PAR.PRESUP*100))%>%
                    select(PROMEDIO, CUMPLIMIENTO, MEDIANA, DESVIACION)
               
               DT::datatable(meta.por.linea, options = list(pageLength = 10))
          })

          
          #Analisis de personal - Imprime personas por funcion
          output$total_puesto <- renderTable({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               result <- tabla.totales%>%
                    group_by(PUESTO)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
          })
          
          #Analisis de personal - Imprime Total por linea de produccion
          output$Totales.por.linea <- renderTable({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               sueldo <- input$sueldo.prom
               
               #calcula produccion promedio real por la eficiencia de balanceo
               efic <- eficiencia.funcion()
               if(is.null(efic)) return(NULL)

               prod.prom <- efic%>%
                    group_by(LINEA)%>%
                    summarise("PROD.REAL" = ceiling(mean(PROD.RESTRICCION)))
               
               
               result <- merge(tabla.renglon, prod.prom, by = "LINEA")%>%
                    mutate("COSTO.PAR" = round((sueldo*PERSONAS)/(PROD.REAL*5),2))%>%
                    select(LINEA, PERSONAS, COSTO.PAR)
               
               return(result)
               
          })
          
          #Analisis de personal - costo promedio de mano de obra
          output$mo.promedio <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               sueldo <- input$sueldo.prom
               
               #calcula produccion promedio real por la eficiencia de balanceo
               efic <- eficiencia.funcion()
               if(is.null(efic)) return(NULL)
               
               prod.prom <- efic%>%
                    group_by(LINEA)%>%
                    summarise("PROD.REAL" = ceiling(mean(PROD.RESTRICCION)))
               
               total.pares <- sum(prod.prom$PROD.REAL)

               result <- merge(tabla.renglon, prod.prom, by = "LINEA")%>%
                    mutate("COSTO.PAR" = round((sueldo*PERSONAS)/(PROD.REAL*5),2),
                           "PRECIO.POND" = COSTO.PAR*PROD.REAL)
               
               
               cat(round(sum(result$PRECIO.POND)/total.pares,2))
               
          })
          #Analisis de personal - incremento en la facturacion por plantilla basica correcta
          output$incr.fact.plantilla <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               
               #calcula produccion promedio real por la eficiencia de balanceo
               efic <- eficiencia.funcion()
               if(is.null(efic)) return(NULL)
               
               prod.prom <- efic%>%
                    group_by(LINEA)%>%
                    summarise("PROD.REAL" = ceiling(mean(PROD.RESTRICCION)))
               
               total.pares <- sum(prod.prom$PROD.REAL)
               
               cat(format(total.pares*5*input$precio.prom, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
               
          })
          #Analisis de personal - facturacion menos costo de mo
          output$fact.mo <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               costo.mo <- sum(tabla.renglon$PERSONAS)*input$sueldo.prom
               
               #calcula produccion promedio real por la eficiencia de balanceo
               efic <- eficiencia.funcion()
               if(is.null(efic)) return(NULL)
               
               prod.prom <- efic%>%
                    group_by(LINEA)%>%
                    summarise("PROD.REAL" = ceiling(mean(PROD.RESTRICCION)))
               
               total.pares <- sum(prod.prom$PROD.REAL)

               cat(format(total.pares*5*input$precio.prom - costo.mo, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
               
          })
          
          #Analisis de personal - facturacion menos costo de mo
          output$total.mo <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               costo.mo <- sum(tabla.renglon$PERSONAS)*input$sueldo.prom
               
               cat(format(costo.mo, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
               
          })
          
          
          #analisiS de personal - Gran total de personas en la plantilla
          output$grantotal <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               cat(round(sum(tabla.totales$PERSONAS)))
          })
     })
     
     
})
