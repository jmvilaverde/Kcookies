#Llamadas a los ficheros auxiliares
source("auxiliar.R")
source("Controller.R")
source("departamentos.R")
source("llamadas.R")
source("logger.R")
source("stats.R")

library(dplyr)



########
#Control
########



main <- function(fase = 1, timemark="0", debug=FALSE, iteracion_sim = 0, llamadasPorDia = 0){

  Constantes.iniciar(fase)
  
  Controller.iniciaObjetosNegocio()
  
  Controller.establecerLlamadasPorDia(llamadasPorDia)
  
  start <- 0
  #end <- 100
  paso <<- 30
  end <- 60*60*13 #13 horas
  #index <- 0
  esPrimeraLlamada <- TRUE
  
  #print(timestamp())
  
  #Control de proceso
  for(i in seq(from=start, to=end, by=paso)){
    situacionHoraria <<- i
    #Obtenemos el número de llamadas que entran en el intervalo
    #Hemos metido un generados de Poisson, ver como se comporta
    numeroLlamadas <- Llamada.getRandomNumeroLlamadas(hora=i)
    
    #print(paste("Paso:",i))
    #Si es mayor que 0 se gestiona
    if (numeroLlamadas!=0){
      #Gestionamos cada llamada
      for(j in 1:numeroLlamadas){
        #Generamos una llamada
        newLlamada <- Llamada.new(hora=i)
        #Si no es la primera llamada lo añadimos a la lista
        if (!esPrimeraLlamada){
          llamadas <<- Llamada.add(llamadas, newLlamada)
        } else { 
          #Si es la primera llamada creamos la lista con el primer elemento
          llamadas <<- newLlamada
          esPrimeraLlamada <- FALSE
        }
        
        
      }
    }

    if (exists("llamadas"))  {
      Controller.ejecutaPaso(debug)
    
      llamadas %>% mutate(Paso=i) -> OneStep.llamadas
      operadores %>% mutate(Paso=i) -> OneStep.operadores
    
      if(!exists("StepByStep.llamadas")){
        StepByStep.llamadas <<- OneStep.llamadas
        StepByStep.operadores <<- OneStep.operadores
      
      } else {
        StepByStep.llamadas <<- rbind(StepByStep.llamadas, OneStep.llamadas)
        StepByStep.operadores <<- rbind(StepByStep.operadores, OneStep.operadores)
      
      }
    }
    
  }
  
  #Metemos en el log las llamadas pendientes
  llamadasAAlmacenar <<- rbind(llamadasAAlmacenar, llamadas)
  
  stats.getStats(Constante.rutaFicherosResultados, timemark)
  
  #print(timestamp())
  porcentajeLlamadasAtendidas <<- sum(llamadasAAlmacenar$atendida)/nrow(llamadasAAlmacenar)*100
  print(paste("Porcentaje de llamadas atendidas:",porcentajeLlamadasAtendidas,"%"))
  #Guardamos un Log con todo el proceso
  Log.saveCompleteLog(timemark)
  
  
  #Datos a pasar a la Simulacion
  sendDataSimulation(iteracion_sim, fase)

  #Limpio las variables para la siguiente simulacion
  clean()
  
  return()
}