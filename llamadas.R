#########
#Llamadas
#########

Llamada.new <- function(hora){
  ran = runif(1, 0, 100)/100
  if(ran <= 0.55) {
    tipo <- "corto"
    duracion <- 60
  }
  else if (ran <= 0.85) {
    tipo <- "media"
    duracion <- 90
  } else {
    tipo <- "larga"
    duracion <- 240
  }
  
  dept <- Departamento.getRandomDepartamento()
  seccion <- Seccion.getSectionByDepartamento(dept)
  
  if(!exists("contadorIdLlamada")){
    contadorIdLlamada <<- 1
  } else {
    contadorIdLlamada <<- contadorIdLlamada+1
  }
  
  tramo <- getTramoHorario(hora)
  
  llamada <- data.frame(contadorIdLlamada, dept, seccion, tipo, duracion, FALSE, 0, 0, 
                        Constante.Tiempo_limite_espera, hora, 0, 0, stringsAsFactors=FALSE, tramo)
  colnames(llamada) <- c("id", "departamento","seccion", "tipo", "duracion", "atendida", "tiempo_espera", "tiempo_atendida", 
                         "limite_espera", "entrada", "salida", "idOperador", "tramoHorario")
  return(llamada)
}

Llamada.add <- function(listaLlamadas, llamada){
  lista <- rbind.data.frame(listaLlamadas, llamada)
  return(lista)
}

Llamada.getRandomNumeroLlamadas <- function(hora, mean=2){
  
  media <- Llamada.getMediaPorHorario(hora)
  
  numeroLlamadas <- rpois(1,media)
  return(numeroLlamadas)
}

Llamada.getMediaPorHorario <- function(hora){
  #Incluimos una constante que usaremos para que nos de el valor esperado
  #Con todas las horas ya funciona bien, no es necesaria
  Constante.FrecuenciaLlamadas <- 1
  
  #Si tenemos una constante con valor mayor que 0 usamos la constante
  if(Constante.llamadasPorDia>0) {
    totalLlamadas <- Constante.llamadasPorDia * Constante.FrecuenciaLlamadas
  } else {
    totalLlamadas <- global.FrecuenciaLlamadas[1,1] * Constante.FrecuenciaLlamadas  
  }
  
  tramoHorario <- as.integer(hora/Constante.FranjaHoraria)+2
  
  
  frecuenciaLlamadas <- as.numeric(gsub(",",".",global.FrecuenciaLlamadas[1,tramoHorario]))
  mediaLlamadas <- as.numeric(totalLlamadas * frecuenciaLlamadas / 100 * (paso/1800))
  return(mediaLlamadas)
}



