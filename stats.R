

stats.histogramLlamadas <- function(calls, titulo="Llamadas entrantes por tramo horario", groupByDepartment = FALSE){

  if(nrow(calls)>0){
    if(!groupByDepartment){
      hist(calls$entrada, xlab = "Paso", ylab= "N. de llamadas", main = titulo, breaks = 26)
      stats.devCopy(titulo)
    } else {
      for(i in global.departamentos$Departamento){
        hist(calls[calls$departamento==i,]$entrada, xlab = "Paso", ylab= "N. de llamadas", main = paste(titulo,i), breaks = 26)
        stats.devCopy(paste(titulo,"Dept_",i,sep=""))
      }
    }
  }
}

stats.plotOperadores <- function(ops, titulo){
  
  ops %>% mutate(ocupacionMedia = (tiempoAtendiendo/(tiempoOcioso+tiempoAtendiendo)*100)) %>% 
    select(id, departamento, tiempoAtendiendo, tiempoOcioso, ocupacionMedia) -> ops
  
  barplot(ops$ocupacionMedia, main=titulo, xlab="id Operador", ylab="% ocupacion", names.arg = ops$id, col = "blue")
  stats.devCopy(titulo)
}

stats.plotOcupacionMediaAgregada <- function(tablaLlamadasAtendidas, titulo){
  barplot(tablaLlamadasAtendidas$porcentajeLlamadasAtendidas,main=titulo, xlab="iteracion", 
          ylab="% ocupacion", names.arg = tablaLlamadasAtendidas$id, col = "blue")
  stats.devCopy(titulo)
}

stats.getStats <- function(ruta, timemark){
  rutaStats <<- ruta
  
  stats.histogramLlamadas(llamadasAAlmacenar, titulo = paste("Llamadas entrantes por tramo horario",timemark), groupByDepartment = FALSE)
  #Agrupadas por departamento
  #stats.histogramLlamadas(llamadasAAlmacenar, titulo = paste("Llamadas entrantes por tramo horario",timemark), groupByDepartment = TRUE)
  stats.histogramLlamadas(llamadasAAlmacenar[llamadasAAlmacenar$atendida,], titulo = paste("Llamadas atendidas por tramo horario",timemark), groupByDepartment = FALSE)
  stats.plotOperadores(operadores, paste("Ocupacion Media",timemark))
  
}

stats.devCopy <- function(titulo){
  dev.copy(png,paste(rutaStats,titulo,".png",sep=""))
  dev.off()
}