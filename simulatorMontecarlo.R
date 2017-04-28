##############################
##############################
## EMBA FEB-2016 F2 GRUPO C ##
##############################
## Hélène Baus              
## Natalia Muñoz
## Tsamajen Serrano
## Hector Sánchez
## Jose María Vilaverde
## Fernando Cañella
##############################
##############################

source("Generator_v2.R")

clearVars <- function(){
  rm(list=setdiff(ls(pos = .GlobalEnv), "tablaLlamadasAtendidas"))
}

saveSimulationData <- function(tabla, n, fase, llamadasPorDia, simname){
  
  ruta <- paste(Constante.rutaFicherosResultados,"AgregadoSimulaciones.csv",sep="")
  
  tablaAgregada <- data.frame(porcentajeLlamadasAtendidas = mean(tabla$porcentajeLlamadasAtendidas),
                              porcentajeMedioOcupacionOperadores = mean(tabla$porcentajeMedioOcupacionOperadores),
                              totalLlamadas = mean(tabla$totalLlamadas), fase = mean(fase), simulacionesEjecutadas = n,
                              llamadasEstimadas = llamadasPorDia, nombreSimulacion = simname)
  
  #tablaAgregada <- tabla  
    
  
  if(file.exists(ruta)) {
    tablaAux <- getCsv(ruta, sep=",")
    tablaAux <- tablaAux[,-1]
    colnames(tablaAux) <- colnames(tablaAgregada)
  }
  
  if(exists("tablaAux")){
    tablaAux <- rbind(tablaAux,tablaAgregada)
  } else {
    tablaAux <- tablaAgregada
  }
  
  write.csv(tablaAux, file=ruta, sep=",", col.names = colnames(tablaAux))
  
}

#fase = Fase que estamos simulando (Fase 1 = No asignación de llamadas a otros departamentos)
#n = Numero de simulaciones a ejecutar
simulatorMontecarlo <- function(fase=1, n=1, simname="test", llamadasPorDia = 0, debug = FALSE){
  
  ini <- timestamp()
  #print(timestamp())
  print("Inicio de proceso")
  
  for(i in 1:n){
    print(paste("Iteracion",i))
    main(fase=fase, timemark=paste(simname,i, sep = "_"), debug = debug, iteracion_sim = i, llamadasPorDia = llamadasPorDia)
    
    clearVars()
    
    if(i==1){
      tablaLlamadasAtendidas   <<- auxTablaLlamadasAtendidas 
      acumuladoSimulacionOperadores <<- auxOperadores
    } else {
      tablaLlamadasAtendidas   <<- rbind(tablaLlamadasAtendidas, auxTablaLlamadasAtendidas )
      acumuladoSimulacionOperadores <<- rbind(acumuladoSimulacionOperadores, auxOperadores)
    }
    
    
  }
  
  print(ini)
  print("Inicio de proceso")
  print(timestamp())
  print("Fin de proceso")
  
  
  
  print(tablaLlamadasAtendidas)
  print(mean(tablaLlamadasAtendidas$totalLlamadas))
  saveSimulationData(tablaLlamadasAtendidas, n, fase, llamadasPorDia, simname)
  
  stats.plotOcupacionMediaAgregada(tablaLlamadasAtendidas, "Ocupacion por Iteracion de la Simulacion")
  
}