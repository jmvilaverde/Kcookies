
#Obtiene el flujo de estados
fases.init <- function(){
  flujo_estados <<- getCsv("./Fases.csv", ";")
}

fase.getFase <- function(fase = "Inicial"){
  fase_a_devolver <- flujo_estados[flujo_estados$Fase==fase,]
  return(fase_a_devolver)
}

fases.getFaseAnterior <- function(fase){
  if(fase!="Inicial"){
    fase_a_devolver <- flujo_estados[flujo_estados$Siguiente_fase==fase,]
  } else {
    fase_a_devolver <- flujo_estados[flujo_estados$Fase==fase,]
  }
  return(fase_a_devolver)
}

#Devuelve la siguiente fase
fase.getSiguienteFase <- function(fase = "Inicial"){
  siguiente_fase <- fase.getFase(fase)$Siguiente_fase
  fase_a_devolver <- flujo_estados[flujo_estados$Fase==siguiente_fase,]  
  return(fase_a_devolver)
}

#Indica la capacidad del recurso limitante para el estado en el que se encuentra el pedido
estado_capacidadRecursoLimitante <- function(estado){
  capacidad <- 0
  
  rec_req <- recursos_getRequeridosParaFase(estado)
  if(length(rec_req)>0){
    for(i in rec_req){
      rec_aux <- recursos_disponibles[recursos_disponibles$Recurso==i,]
      if(nrow(rec_aux)>0){
        for(j in 1:nrow(rec_aux)){
          if(rec_aux[j,]$Capacidad < capacidad || capacidad == 0){
            capacidad <- rec_aux[j,]$Capacidad
          }
        }  
      }
    }
  }

  return(capacidad)
}