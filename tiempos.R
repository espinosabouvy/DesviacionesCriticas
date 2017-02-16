
#limpia el archivo que sale del sistema, se hace tabla dinamica  presenta 4 columnas
#DEPTO, NLISTA, VCESTIL, FUNCION, TIEMPO, PARES, FAMILIA
#DEBE REALIZAR (DEJAR ESTILO, TIEMPOS PARA CADA FUNCION (AGRUPANDO PESP-PREL))
preparar <- function(agrupar = FALSE){
     require(tidyr)
     require(dplyr)
     
     tiempos.raw <- read.csv("tiempos.csv")
     
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
     tiempos.raw$FUNCION <- ifelse(grepl("CA-COR", 
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
     tiempos.raw <<- tiempos.raw
     
     #dejar las funciones relevantes
     funciones <- c("PESPUNTADOR","PRELIMINAR","CORTADOR FLASH", "CORTADOR.FORRO","CORTADOR PIEL",
                    "DOBLILLADOR", "REBAJADOR")
     
     if(agrupar){
          #agrupar por estilo, sin importar departamento (una sola linea de produccion de corte, prep, fam)
          tiempos.funcion <<- tiempos.raw%>%
               filter(FUNCION %in% funciones)%>%
               select("ESTILO" = VCESTIL, FUNCION, TIEMPO)%>%
               group_by(ESTILO, FUNCION)%>%
               summarise("TIEMPO" = sum(TIEMPO))%>%
               spread(FUNCION,TIEMPO)
     } else {
          #agrupar por departamento, estilo
          tiempos.funcion <<- tiempos.raw%>%
               filter(FUNCION %in% funciones)%>%
               select(DEPTO, "ESTILO" = VCESTIL, FUNCION, TIEMPO)%>%
               group_by(DEPTO, ESTILO, FUNCION)%>%
               summarise("TIEMPO" = sum(TIEMPO))%>%
               spread(FUNCION,TIEMPO)
     }
     
     write.csv(tiempos.funcion, "Tiempos-funcion.csv", row.names = F)
}