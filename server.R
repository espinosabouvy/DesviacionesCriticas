
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)



shinyServer(function(input, output, session) {
     options(shiny.maxRequestSize=15*1024^2)
     
     #tabla vacia para cargar pares por linea
     tb.porproduc <- data.frame("Linea" = numeric(0), "Pares" = numeric(0))
     

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
          
 
          #llenar combo con el archivo preparado
          output$depto.select <- renderUI({
               deptos <- unique(tiempos$DEPTO)
               selectInput("depto.selected", "Seleccionar departamento", as.list(deptos))
          })
          
          #llenar combo con el archivo preparado
          output$fams.select <- renderUI({
               fams <- unique(tiempos$LINEA)%>%sort()
               selectInput("fams.selected", "Filtrar lineas de produccion", as.list(fams), multiple = TRUE)
          })
          
          
          return(tiempos)
     })
          
 
          
     #TABLA COMPLETA DE ESTILOS Y TIEMPOS
     output$tabla_completa <- DT::renderDataTable({
          tabla.raw <- reporte.final()
          if(is.null(tabla.raw)) return(NULL)

          DT::datatable(tabla.raw, options = list(pageLength = 25))
     })
     
     
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
          

          #convertir los tiempos en personas utilizando sliders (para 1000 pares por dia)
          if (input$personas) {
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               datos <-  datos%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*1000/(efic*hrs*3600)))%>%
                    select(DEPTO, ESTILO, LINEA, FUNCION, "TIEMPO" = PERSONAS)
          }
          
          datos <- datos%>%
               filter(LINEA %in% input$fams.selected)
          
          #completar con cero las funciones que existen en cada depto y el estilo no las tiene
          #primero se debe agregar todos los estilos a todas los deptos
          estilos.fam <<- unique(datos[datos$DEPTO == "FAMILIA",2:3])
          
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

     # Drop-down de linea a definir pares por producir
     output$lineas.selected <- renderUI({
          num.fam <- reporte.final()
          if(is.null(num.fam)) return(NULL)
          
          l.linea <- unique(num.fam$LINEA)%>%sort()
          selectInput("linea.seleccionada", "Linea de produccion", as.list(l.linea))
     })
     
     actualiza <- eventReactive(input$agregar, {
          #agregar a la tabla los pares
          l.linea <- input$linea.seleccionada
          pares <- as.numeric(input$pares)
          
          temp <- data.frame("LINEA" = l.linea, "PARES" = pares)
          
          #quita la linea que existe (actualizar)
          tb.porproduc <- tb.porproduc%>%filter(LINEA != l.linea)
          tb.porproduc <<- rbind(tb.porproduc, temp)%>%arrange(LINEA)
     })
     
     output$por.producir <- DT::renderDataTable({

          tabla <- actualiza()               
          DT::datatable(tabla, options = list(dom = 't'))
     })
     
     # Drop-down de linea a filtrar
     output$seleccion_linea <- renderUI({
          num.fam <- reporte.final()
          if(is.null(num.fam)) return(NULL)
          l.linea <- unique(num.fam$LINEA)%>%sort()
          selectInput("dataset", "Filtrar por linea de produccion", as.list(l.linea))
     })
     
     free.scale.fin <- reactive({
          b.scales = "fixed"
          if (input$same.scale.fin){
               b.scales = "free"
          }
          return(b.scales)
          
     })
     

     #grafico por linea, dependen del combobox cual mostrar
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
     
     #imprimir los estilos criticos segun el slider de porcentaje en los limites
     output$cancelar.criticos  <- renderTable({
          for.plot <- obtener.criticos()
          
          if (is.null(for.plot)) return(NULL)
          
          temp <- for.plot%>%
               filter(DESVIACION == "CRITICO")%>%
               select(ESTILO, LINEA)
          
          criticos <- unique(temp) 
          
          return(criticos)
          
     })
     
     #grafico por linea, dependen del combobox cual mostrar
     output$plot.por.linea <- renderPlotly({
          for.plot <- obtener.criticos()
          
          if (is.null(for.plot)) return(NULL)
          
          b.scales <- free.scale.fin()
          
          #quitar del analisis los criticos
          
          
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
               ylab(unidades)
          )
     })
     
     #ANALISIS FINAL
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
     
     #indicador general de desviacion (imprimir)
     output$indicador.desviacion <- renderTable({
          tabla <- fun.indicador.desviacion()
          if (is.null(tabla)) return(NULL)
          temp <- tabla
          return(temp)
     })
     
     #calcula tabla de desviaciones
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
     
     output$pct.mejora <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          anterior <- fun.indicador.desviacion()
          if(is.null(anterior)) return(NULL)
          
          ant <- anterior%>%
               filter(LINEA == input$dataset)
          
          cat(as.numeric(100-round(mejora$NvaDesviacion/ant[5]*100,2)))
     
     })
     
     output$nva.desviacion <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          cat(as.numeric(mejora$NvaDesviacion))
     })
     

     output$incr.produccion <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          anterior <- fun.indicador.desviacion()
          if(is.null(anterior)) return(NULL)
          
          incr <- round((1-(mejora$Maximo/anterior%>%
                                 filter(LINEA == input$dataset)%>%
                                 select(Maximo)))*100,2)
          #incr <- anterior%>%filter(LINEA == input$dataset)%>%select(Maximo)
          
          cat(as.numeric(incr))
     })

     
     #indicador general de desviacion sin criticos
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
     
     output$desviaciones <- DT::renderDataTable({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
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
          }
     })
     
     
     # Drop-down de departamentos, para seleccionar para flujos
     output$flujo.deptos <- renderUI({
          deptos <- lectura.inicial()
          if(is.null(deptos)) return(NULL)
          un.deptos <- unique(deptos$DEPTO)%>%sort()
          selectInput("cb.deptos.flujo", "Selecciona departamentos para revisar", as.list(un.deptos),
                      multiple = T)
     })
     
     # Drop-down linea para revisar flujo
     output$flujo.linea <- renderUI({
          datos <- reporte.final()
          if(is.null(datos)) return(NULL)
          
          lineas <- unique(datos$LINEA)%>%sort()
          selectInput("cb.lineas.flujo", "Selecciona una linea", as.list(lineas),
                      multiple = F)
     })
     
     # Drop-down de estilos para flujo
     output$flujo.estilo <- renderUI({
          datos <- reporte.final()
          if(is.null(datos)) return(NULL)
          
               datos <- datos%>%filter(LINEA == input$cb.lineas.flujo)
               estilos <- unique(datos$ESTILO)%>%sort()
               selectInput("cb.estilos.flujo", "Selecciona un estilo", as.list(estilos),
                           multiple = T)
     })
     
     reporte.flujo <- reactive({
          datos <- lectura.inicial()
          if (is.null(datos)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          #leer combo y definir lineas a usar, sirve para quitar estilos sin linea asignada
          #y para quitar canteras o maquilas, dan problemas al agrupar en un solo depto pues
          # en realidad es no se procesa junto
          if (is.null(input$fams.selected)) return(NULL)
          
          
          #convertir los tiempos en personas para 1000 pares al dia
          #if (input$personas){
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               datos <-  datos%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*1000/(efic*hrs*3600)))%>%
                    select(DEPTO, ESTILO, LINEA, FUNCION, PERSONAS)
          #}
          
          #lineas seleccionadas
          datos <- datos%>%
               filter(LINEA %in% input$cb.lineas.flujo)

          
          #completar con cero las funciones que existen en cada depto y el estilo no las tiene
          #primero se debe agregar todos los estilos a todas los deptos
          estilos.fam <<- unique(datos[datos$DEPTO == "FAMILIA",2:3])
          
          #obtener metas por departamento, seleccionar depto que se programa
          #ese debe ser 100 % segun su plantilla y de ahi revisar que meta queda en 
          #los otros departamentos segun su plantilla usando una regla de 3 (posteriormente)
          #se puede mejorar usando las funciones
          
          #crear bd vacia
          bd <- data.frame("ESTILO" = numeric(0),
                           "LINEA" = numeric(0),
                           "FUNCION"= numeric(0),
                           "PERSONAS" = numeric(0),
                           "DEPTO" = numeric(0))
          tiempos <- datos
          
          #calcula y completa con cero el personal de cada estilo
          for (i in input$cb.deptos.flujo){
               #se pueden seleccionar varios deptos - es lo mas logico
               temp <- tiempos%>%
                    filter(DEPTO == i)%>%
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
          completa <- merge(bd, plantilla , by = c("LINEA","DEPTO","FUNCION"))%>%
               mutate("META" = ifelse(PERSONAS == 0, 300, ceiling((PLANTILLA/PERSONAS)*100)))
          
          return(completa)
          
          
     })  
     
     
     
     full.flujo <- reactive({
          
               temp <- reporte.flujo()
               if(is.null(temp)) return(NULL)
               
               estilo = input$cb.estilos.flujo
               if (length(estilo) == 0) return(NULL)
               
               #cuantos estilos seleccionados
               cuantos <- length(input$cb.estilos.flujo)
               
               #acumular meta de los estilos seleccionados si es mas de uno
               if (cuantos > 1){
                    tabla.plot <- temp%>%
                         filter(ESTILO == input$cb.estilos.flujo | ESTILO == "AGRUPADO")%>%
                         mutate("Meta.Parcial" = META/cuantos)%>%
                         group_by(LINEA, DEPTO, FUNCION)%>%
                         summarise("Pct.meta" = sum(Meta.Parcial),
                                   "PERSONAS" = ceiling(sum(PERSONAS)/cuantos),
                                   "PLANTILLA" = mean(PLANTILLA))%>%
                         mutate("ESTILO" = "AGRUPADO")
               } else {
                    
                    tabla.plot <- temp%>%
                         group_by(LINEA)%>%
                         filter(ESTILO == input$cb.estilos.flujo | ESTILO == "AGRUPADO")%>%
                         mutate("Pct.meta" = META)
               }
               
               plot.final <- tabla.plot%>%
                    arrange(LINEA, DEPTO, FUNCION, Pct.meta)%>%
                    mutate("DEPTOFUNC" = paste(DEPTO,"/",FUNCION),
                           "EFIC" = round(min(Pct.meta)/Pct.meta,2),
                           "DIF" = PERSONAS - PLANTILLA)
               
               return(plot.final)
               
     })

     #tabla del grafico
     output$tabla.plot <- DT::renderDataTable({
          plot.final <- full.flujo()

          if (is.null(plot.final))  return(NULL)
          DT::datatable(plot.final, options = list(pageLength = 25))
     })
     
     #tabla del grafico balanceada
     output$balanceo <- DT::renderDataTable({
          plot.final <- full.flujo()
          
          if (is.null(plot.final))  return(NULL)
          
          cumplimiento = min(plot.final$Pct.meta)
          eficiencia = mean(plot.final$EFIC)
          
          # #buscar cumplimineto y eficiencia mayor al 90 o 10 iteraciones
          # while(cumplimiento < 90 | eficiencia < .9){
          #      donador <- min(plot.final$DIF)
          #      recibe <- min(plot.final$Pct.meta)
          #      
          #      #en el minimo, agrega una persona y recalcula meta
          #      nva.plantilla.mas <- plot.final[plot.final$Pct.meta == recibe,]$PLANTILLA + 1
          #      personas <- plot.final[plot.final$Pct.meta == recibe,]$PERSONAS
          #      nva.efic.mas <- round(nva.plantilla.mas /personas,2)
          #      
          #      
          #      #actualizado
          #      cumplimiento = min(plot.final$Pct.meta)
          #      eficiencia = mean(plot.final$EFIC)
          # }
               
          DT::datatable(plot.final, options = list(pageLength = 25))
     })
     
     
     output$cumpl.meta <- renderPrint({
          plot.final <- full.flujo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(min(plot.final$Pct.meta))),"%")
     })

     output$ef.esperada <- renderPrint({
          plot.final <- full.flujo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(mean(plot.final$EFIC)*100),"%"))
     })

     observeEvent(input$cb.estilos.flujo, {

          output$plot.flujo <- renderPlotly({
               
               plot.final <- full.flujo()
               if (is.null(plot.final))  return(NULL)
               
               #METAS MINIMAS
               intercepts <- plot.final%>%
                    group_by(ESTILO, DEPTO)%>%
                    filter(Pct.meta > 0)%>%
                    summarise("Meta.real" = min(Pct.meta))
               
               ggplot(plot.final, aes(DEPTOFUNC, Pct.meta, colour = ESTILO, group = 1)) + 
                    geom_point(size = 2) + geom_line() + 
                    scale_x_discrete(labels = substr(plot.final$FUNCION,1,3)) +
                    geom_hline(data = intercepts, aes(yintercept =  Meta.real, colour = DEPTO))+ 
                    expand_limits(y = c(0,100))
               #coord_cartesian(ylim = c(0,200)) 
          })
          
     })
     
     
     #aqui esta todo lo que requiere pares por linea - observa el boton agregar y actualiza
     observeEvent(input$agregar, {
          
          #calcula con los pares por linea - si no, no calcula
          output$inc.facturacion <- renderPrint({
               mejora <- desviacion.mejorada()
               if(is.null(mejora)) return(NULL)
               
               anterior <- fun.indicador.desviacion()
               if(is.null(anterior)) return(NULL)
               
               incr <- round((1-(mejora$Maximo/anterior%>%
                                      filter(LINEA == input$dataset)%>%
                                      select(Maximo)))*100,2)
               
               #si no hay pares por producir por linea no hace el calculo
               if(nrow(tb.porproduc)==0) return(NULL)
               
               l.actual <- input$dataset
               par.fam <- tb.porproduc%>%
                    filter(LINEA == l.actual)%>%
                    select(PARES)
               
               if (nrow(par.fam)==0) return(cat("Sin pares por producir"))
               
               mas.fact <- as.numeric(ceiling((par.fam * (incr/100) * as.numeric(input$precio.prom))*5))

               cat(format(mas.fact, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
          })
          

          #personal por linea
          output$Porlinea <- DT::renderDataTable({
               temp <- reporte.final()
               if(is.null(temp)) return(NULL)
               #si no hay pares por producir por linea no hace el calculo
               if(nrow(tb.porproduc)==0) return(NULL)
               
               fin <- dim(temp)[2]
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               familias <- max(as.numeric(temp$LINEA))
               prs <- input$pares.hora/familias
     
               tabla.renglon <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
                    group_by(LINEA, PUESTO)%>%
                      summarise("TIEMPO" = round(ifelse(input$personas, 
                                                        mean((TIEMPO/1000)*(efic*hrs*3600)),
                                                        mean(TIEMPO)),2))%>%
                    merge(tb.porproduc, by = "LINEA")%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)))
               
               DT::datatable(tabla.renglon, options = list(pageLength = 50))
               
          })
     
          
          #personal por puesto
          output$total_puesto <- renderTable({
               temp <- reporte.final()
               if(is.null(temp)) return(NULL)
               #si no hay pares por producir por linea no hace el calculo
               if(nrow(tb.porproduc)==0) return(NULL)
               
               
               fin <- dim(temp)[2]
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               familias <- max(as.numeric(temp$LINEA))
               prs <- input$pares.hora/familias
               
               tabla.puesto <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("TIEMPO" = round(ifelse(input$personas, 
                                                      mean((TIEMPO/1000)*(efic*hrs*3600)),
                                                      mean(TIEMPO)),2))%>%
                    merge(tb.porproduc, by = "LINEA")%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)))%>%
                    group_by(PUESTO)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
          })
          
          #personal total por puesto
          output$Totales.por.linea <- renderTable({
               temp <- reporte.final()
               if(is.null(temp)) return(NULL)
               #si no hay pares por producir por linea no hace el calculo
               if(nrow(tb.porproduc)==0) return(NULL)
               
               
               fin <- dim(temp)[2]
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               familias <- max(as.numeric(temp$LINEA))
               prs <- input$pares.hora/familias
               
               totales.puesto <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("TIEMPO" = round(ifelse(input$personas, 
                                                      mean((TIEMPO/1000)*(efic*hrs*3600)),
                                                      mean(TIEMPO)),2))%>%
                    merge(tb.porproduc, by = "LINEA")%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)))%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = ceiling(sum(PERSONAS)))
               
          })
          
          #gran total
          output$grantotal <- renderPrint({
               temp <- reporte.final()
               if(is.null(temp)) return(NULL)
               #si no hay pares por producir por linea no hace el calculo
               if(nrow(tb.porproduc)==0) return(NULL)
               
               
               fin <- dim(temp)[2]
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               familias <- max(as.numeric(temp$LINEA))
               prs <- input$pares.hora/familias
               
               tabla.totales <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("TIEMPO" = round(ifelse(input$personas, 
                                                      mean((TIEMPO/1000)*(efic*hrs*3600)),
                                                      mean(TIEMPO)),2))%>%
                    merge(tb.porproduc, by = "LINEA")%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)))%>%
                    summarise("PERSONAS" = ceiling(sum(PERSONAS)))
               cat(sum(tabla.totales$PERSONAS))
          })
     })
     
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
                         #ggtitle("Dispersion de tiempo (segundos) para producir un par")+
                         theme(axis.text=element_text(size=8))
               )
          } 
          
          
     })
     
     
})
