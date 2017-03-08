#despues de tener la tabla dinamica que sale del sistema de tiempos - estilos habilitados
#se debe separar en archivos por departamento y dejar la tabla con el formato
#estilo - familia - puesto1 - puesto2 - puestox

preparar <- function(exportar = FALSE){
     require(dplyr)
     require(tidyr)
     
     #el formato inicial es
     #limpiar el formato, actual (LINEA, VCESTIL, PARES, FAMPESP, FAMMONT, DEPTO, FUNCION, TIEMPO,
     #PERSONAS, META)

     tiempos.raw <- read.csv("FUNCIONES_ESTILOS_HABILITADOS.csv", stringsAsFactors = TRUE)
     
     deptos.usar <- c("CORTE","CORTE Y PREPARA", "FAMILIA", "FORRADOS", "PLANTA", "RAYADO Y RESACA",
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
     tiempos <<- tiempos.raw%>%
          select(DEPTO, "ESTILO" = VCESTIL, FAMPESP, FUNCION, "TIEMPO" = TIEMPO)
     
     
     #completar con cero las funciones que existen en cada depto y el estilo no las tiene
     #primero se debe agregar todos los estilos a todas los deptos
     
     estilos.fam <<- unique(tiempos[tiempos$DEPTO == "FAMILIA",2:3])
     
     #hacer esto cuando se seleccione el depto
     for(i in unique(tiempos$DEPTO)){
          temp <- tiempos%>%
                    filter(DEPTO == i)%>%
                    select(ESTILO, FUNCION, TIEMPO)%>%
                    group_by(ESTILO, FUNCION)%>%
                    summarise("TIEMPO" = sum(TIEMPO))%>%
                    spread(FUNCION,TIEMPO, drop = FALSE, fill = 0)
          result <<- merge(estilos.fam, temp, by = "ESTILO")
          
          if (exportar){
               write.csv(result, paste0(i, ".csv"), row.names = F)
          }
     }
}