Log.saveCompleteLog <- function(timemark){
  log(llamadasAAlmacenar, "llamadas",timemark)
  log(operadores, "operadores",timemark)
  log(StepByStep.llamadas, "StepLlamadas", timemark)
  log(StepByStep.operadores, "StepOperadores",timemark)
}

log <- function(obj, nombre, timemark){
  ruta = paste(Constante.rutaFicherosResultados,"log",nombre,timemark,".csv",sep="")
  write.csv(obj, file=ruta, sep=";")
}