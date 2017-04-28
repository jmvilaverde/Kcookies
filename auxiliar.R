#####################
#Funciones Auxiliares
#####################

getCsv <- function(ruta, separador=";"){
  df = read.csv(ruta, sep=separador, stringsAsFactors = FALSE)
  return(df)
}

horas_to_minutos <- function(hora){
  hora_aux <- as.integer(substr(hora,1,2))
  minutos_aux <- as.integer(substr(hora,4,5))
  minutos <- minutos_aux + hora_aux*60
  return(minutos)
}

minutos_to_horas <- function(minutos){
  hora_aux <- aux_normaliza_hora(as.character(minutos%/%60))
  #if(nchar(hora_aux)==1) hora_aux <- paste("0", hora_aux, sep="")
  
  minutos_aux <- aux_normaliza_hora(as.character(minutos%%60))
  #if(nchar(minutos_aux)==1) minutos_aux <- paste("0", minutos_aux, sep="")
  
  hora <- paste(hora_aux,minutos_aux, sep=":")
  return(hora)
}

aux_normaliza_hora <- function(cadena){
  if(nchar(cadena)==1) cadena <- paste("0", cadena, sep="")
  return(cadena)
}


######################
