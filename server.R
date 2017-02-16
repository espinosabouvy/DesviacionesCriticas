
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(cluster)


shinyServer(function(input, output, session) {

     output$download <- downloadHandler(
          
          filename = function() { paste("Modelos asignados por linea", '.csv', sep='') },
          content = function(file) {
               write.csv(datasetInput(), file, row.names = F)
          }
     )
     
     reporte.final <- reactive({

          
          inFile <- NULL
          inFile <- input$browse          
          #inFile$datapath <- "tiempos.csv"
          
          if (is.null(inFile)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          #leer por tipo de archivo
          datos <- read.csv(inFile$datapath, 
                            header = input$header, na.strings = c("NA",""))
          
          #agrupar en una sola linea
          #cambia los datos de la columna linea todo a 1
          if(input$agrupado){
               datos[,2] <- 1
          }
          
          #convertir NAS en cero
          datos[is.na(datos)]<-0
          names(datos)[1] <- "ESTILO"
          names(datos)[2] <- "LINEA"
          #estilo y linea como factor
          datos[,1] <- as.factor(datos[,1])
          datos[,2] <- as.factor(datos[,2])
          #nas a cero
          datos[is.na(datos)]<-0
          
          #limitar cantidad de modelos
          #datos<- datos[1:20,]
          
          return(datos)
          
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
          
          ggplotly(
          ggplot(for.plot, aes(ESTILO,TIEMPO, colour = DESVIACION)) +
               geom_point() +
               facet_wrap(~PUESTO, ncol=2, strip.position = "right" ,scales = b.scales, as.table = T) +
               theme(strip.background = element_blank(), strip.placement = "outside") + 
               geom_hline(data = for.plot%>%
                               group_by(PUESTO)%>%
                               summarise("Promedio" = mean(TIEMPO)),
                          aes(yintercept = Promedio), col = "red", lwd = 1)
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
          
          cat(as.numeric(100-round(mejora/ant[5]*100,2)))
     
     })
     
     output$nva.desviacion <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          cat(as.numeric(mejora))
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
                              "NvaDesviacion" = round((Maximo-Minimo)/Promedio*100,2))%>%
                    select(NvaDesviacion)
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
     
     #personal por linea
     output$Porlinea <- DT::renderDataTable({
          temp <- reporte.final()
          if(is.null(temp)) return(NULL)
          fin <- dim(temp)[2]
          efic <- input$eficiencia/100
          hrs <- input$horas.trabajo
          familias <- max(as.numeric(temp$LINEA))
          prs <- input$pares.hora/familias
          
          tabla.renglon <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
               group_by(LINEA, PUESTO)%>%
               summarise("TIEMPO.PROMEDIO" = round(mean(TIEMPO),2))%>%
               mutate("PERSONAS" = ceiling(TIEMPO.PROMEDIO*prs/(efic*hrs*3600)))
          DT::datatable(tabla.renglon, options = list(pageLength = 50))
          
     })
     
     #personal por puesto
     output$total_puesto <- renderTable({
          temp <- reporte.final()
          if(is.null(temp)) return(NULL)
          fin <- dim(temp)[2]
          efic <- input$eficiencia/100
          hrs <- input$horas.trabajo
          familias <- max(as.numeric(temp$LINEA))
          prs <- input$pares.hora/familias
          
          tabla.puesto <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
               group_by(LINEA, PUESTO)%>%
               summarise("TIEMPO.PROMEDIO" = round(mean(TIEMPO),2))%>%
               mutate("PERSONAS" = ceiling(TIEMPO.PROMEDIO*prs/(efic*hrs*3600)))%>%
               group_by(PUESTO)%>%
               summarise("PERSONAS" = sum(PERSONAS))
          
     })
     
     #personal total por puesto
     output$Totales <- renderTable({
          temp <- reporte.final()
          if(is.null(temp)) return(NULL)
          fin <- dim(temp)[2]
          efic <- input$eficiencia/100
          hrs <- input$horas.trabajo
          familias <- max(as.numeric(temp$LINEA))
          prs <- input$pares.hora/familias
          
          tabla.totales <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
               group_by(LINEA, PUESTO)%>%
               summarise("TIEMPO" = round(mean(TIEMPO),2))%>%
               mutate("PERSONAS" = ceiling(TIEMPO*prs/(efic*hrs*3600)))%>%
               summarise("PERSONAS" = ceiling(sum(PERSONAS)))
          
     })
     
     #gran total
     output$grantotal <- renderPrint({
          temp <- reporte.final()
          if(is.null(temp)) return(NULL)
          
          fin <- dim(temp)[2]
          efic <- input$eficiencia/100
          hrs <- input$horas.trabajo
          familias <- max(as.numeric(temp$LINEA))
          prs <- input$pares.hora/familias
          
          tabla.totales <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
               group_by(LINEA, PUESTO)%>%
               summarise("TIEMPO" = round(mean(TIEMPO),2))%>%
               mutate("PERSONAS" = ceiling(TIEMPO*prs/(efic*hrs*3600)))%>%
               summarise("PERSONAS" = ceiling(sum(PERSONAS)))
          cat(sum(tabla.totales[,2]))
     })
     
     output$grafico.final <- renderPlotly({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
               #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
               fin <- dim(reporte)[2]
               tabla.renglon <- gather(reporte, "PUESTO","TIEMPO",c(3:fin))
               
               # grafico de desviaciones por puesto
               ggplotly(
                    ggplot(data = tabla.renglon, aes(ESTILO, TIEMPO, colour = LINEA)) + 
                         geom_point() + 
                         facet_grid(PUESTO~., as.table = F, scales = "free") +
                         xlab("Estilos") +
                         ylab("Segundos por par")  +
                         #ggtitle("Dispersion de tiempo (segundos) para producir un par")+
                         theme(axis.text=element_text(size=8))
               )
          } 
          
          
     })
     

     #TABLA COMPLETA DE ESTILOS Y TIEMPOS
     output$tabla_completa <- DT::renderDataTable({
          tabla.raw <- reporte.final()
          if(is.null(tabla.raw)) return(NULL)
          
          DT::datatable(tabla.raw, options = list(pageLength = 10))
     })
     
})
