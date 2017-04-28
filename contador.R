
contador.init <- function(inicio="20:00", fin="22:00", paso_minuto = 1){
  hora_inicio <<- horas_to_minutos(inicio)
  hora_fin <<- horas_to_minutos(fin)
  
  if(hora_inicio < hora_fin){
    num_pasos <<- hora_fin - hora_inicio  
  }else{
    num_pasos <<- 0
  }
  paso <<- paso_minuto
}

contador.getMinutosTurno <- function(){
  return(num_pasos)
}