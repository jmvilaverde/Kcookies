###########
#Constantes
###########

Constantes.iniciar <- function(fase){
  
  
  Constante.Tiempo_limite_espera <<- 120
  Constante.OcupacionMaxima <<- 0.8 #Todavia no se usa
  #Para llevar el conteo del id de las llamadas. Se reinicia en cada ejecucion.
  contadorIdLlamada <<- 0
  
  #Estados de Operadores
  Constante.OperadorOcupado <<- "ocupado"
  Constante.Libre <<- "libre"
  
  #Fase de la Simulacion
  #Fase 1 -> Se puede enviar a otras secciones
  #Fase 2 -> Cada Operador solo puede atender las de su seccion
  Constante.FaseSimulacion <<- fase
  
  #Rutas de ficheros
  Constante.rutaFicherosConfiguracion <<- "/config"
  Constante.rutaConfigDepartamentos <<- paste(".",Constante.rutaFicherosConfiguracion,"/configDepartamentosFase",Constante.FaseSimulacion,".csv",sep ="")
  Constante.rutaConfigOperadores <<- paste(".",Constante.rutaFicherosConfiguracion,"/configOperadoresFase",Constante.FaseSimulacion,".csv",sep ="")
  Constante.rutaConfigHorarios <<- paste(".",Constante.rutaFicherosConfiguracion,"/configHorarios.csv",sep ="")
  Constante.rutaFrecuenciaLlamadas <<- paste(".",Constante.rutaFicherosConfiguracion,"/configFrecuenciaLlamadas.csv",sep ="")
  
  
  Constante.rutaFicherosResultados <<- "./resultados/"
  
  Constante.FranjaHoraria <<-1800 #1800 segundos = 30 minutos cada franja
  
  #Si la Fase es la 1 o 2 se puede enviar a otras Secciones
  #Si la Fase es la 3 no se puede enviar a otras secciones
  if(Constante.FaseSimulacion==1 || Constante.FaseSimulacion ==2){
    envioOtrasSecciones <<- FALSE
  } else if(Constante.FaseSimulacion==3){
    envioOtrasSecciones <<- TRUE
  }
  
  
}


###########
#Controller
###########

Controller.establecerLlamadasPorDia <- function(llamadasPorDia=0){
  Constante.llamadasPorDia <<- llamadasPorDia
}

Controller.iniciaObjetosNegocio <- function(){
  
  
  
  #Crear el CallCenter y obtener la lista de operadores
  operadores <<- Operador.createCallCenter(Constante.rutaConfigOperadores)
  
  #Obtiene los departamentos del fichero de configuracion
  global.departamentos <<- Departamento.getDepartamentosFromCSV(Constante.rutaConfigDepartamentos)
  
  global.Horarios <<- Horarios.getHorariosFromCSV(Constante.rutaConfigHorarios)
  
  global.FrecuenciaLlamadas <<- Horarios.getFrecuenciaLlamadasFromCSV(Constante.rutaFrecuenciaLlamadas)
  
}



Controller.ejecutaPaso <- function(debug=FALSE){
  
  library(dplyr)
  
  ops <- operadores
  calls <- llamadas
  
  #Comprobación de horario de trabajo
  ops <- Operador.controlHorarioTrabajo(ops, global.Horarios)
  
  
  #Si hay operadores libres asignamos llamadas
  operadoresLibres <- Operador.getNumeroOperadorLibres(ops)
  
  #Si hay operadores libres y llamadas para procesar
  if(operadoresLibres>0 && nrow(calls)>0){
    
    #Asigna llamadas pendientes a los operadores en estado libre
    for(i in 1:nrow(calls)){
      idOperadorLibre <- 0
      
      if (identical(calls[i,]$atendida,FALSE)){
        
        #dept <- calls[i,]$departamento
        #Cambio, departamento es indicativo literal, nos guiamos por seccion
        seccion <- as.integer(calls[i,]$seccion)
        
        #Mira si el departamento requerido tiene operadores disponibles y tomamos el que tiene menos ocupación
        #idOperadorTemp <- Operador.getOperadorLibreByDepartamento(ops, dept)
        #Cambio, departamento es indicativo literal, nos guiamos por seccion
        
        #Mira si la seccion tiene operadores disponibles y tomamos el que tiene menos ocupación
        idOperadorLibre <- Operador.getOperadorLibreBySeccion(ops, seccion)
        
        
        #Si no hemos encontrado operador en el departamento y se puede enviar a otras secciones
        if (idOperadorLibre == 0 && identical(envioOtrasSecciones, TRUE)){
          
          #Mira que departamentos están por debajo de su ocupación máxima
          ops %>% group_by(departamento) %>% summarize(cuenta=n()) -> totalDept
          filter(ops, estado=="libre") %>% group_by(departamento) %>% summarize(cuenta=n()) -> totalLibresDept
          inner_join(totalDept,totalLibresDept, by="departamento") %>% mutate(ratioOcup=1-(cuenta.y/cuenta.x)) %>%
            arrange(ratioOcup) %>% select(departamento) %>% head(1) %>% as.character() -> departamentoMasDesocupado
          
          filter(ops, departamento == departamentoMasDesocupado, estado == "libre") %>% 
            arrange(desc(tiempoAtendiendo)) %>% select(id) %>% head(1) -> idOperadorTemp
          
          if (nrow(idOperadorTemp) > 0) idOperadorLibre <- as.integer(idOperadorTemp[1])
          
        }
        
        if (idOperadorLibre > 0){
          
          #print(paste(c("Asignamos el operador ", idOperadorLibre, " la llamada ", i)))
          ops[idOperadorLibre,]$estado <- Constante.OperadorOcupado
          ops[idOperadorLibre,]$idLlamadaAtendiendo <- i
          calls[i,]$atendida <- TRUE
          calls[i,]$idOperador <- idOperadorLibre
          
        }
      }
      
    }#Fin del For
    
  }#Fin de si hay operadores libres y llamadas a procesar
  
  
  
  ##############################
  #Ejecutamos el paso de tiempo
  ##############################
  
  ######################################
  #Añado tiempo a los operadores ociosos
  ######################################
  
  for(i in 1:nrow(ops)){
    #Añado tiempo en estado ocioso a los que están en estado libre
    if(as.character(ops[i,]$estado) == "libre"){
      ops[i,]$tiempoOcioso <- ops[i,]$tiempoOcioso + paso
    }
  }
  
  if(nrow(calls)>0){
    #Incremento los tiempos de las llamadas
    
    #Incremento el tiempo de espera de las llamadas que no han sido atendidas
    llamadasNoAtendidas <- !calls$atendida & calls$salida==0
    calls[llamadasNoAtendidas,]$tiempo_espera <- calls[llamadasNoAtendidas,]$tiempo_espera + paso
    
    #Quito las que hayan superado el tiempo de espera
    llamadasQueSuperan <- (calls$tiempo_espera >= calls$limite_espera) & calls$salida==0 & !calls$atendida
    calls[llamadasQueSuperan,]$salida <- calls[llamadasQueSuperan,]$entrada + calls[llamadasQueSuperan,]$tiempo_espera
    
    #Incremento las llamadas que están siendo atendidas
    llamadasAtendidas <- calls$atendida & calls$salida==0
    calls[llamadasAtendidas,]$tiempo_atendida <- calls[llamadasAtendidas,]$tiempo_atendida + paso
    
    #Incrementa el tiempo de los operadores que están atendiendo llamadas
    operadoresAtendiendo <- ops$estado =="ocupado"
    ops[operadoresAtendiendo,]$tiempoAtendiendo <- ops[operadoresAtendiendo,]$tiempoAtendiendo + paso
    
    #Obtengo las llamadas que han terminado en este paso
    llamadasAcabadas <- (calls$tiempo_atendida >= calls$duracion) & calls$salida == 0
    #Si hay llamadas acabadas
    if(sum(llamadasAcabadas)>0){
      #Las pongo hora de salida
      calls[llamadasAcabadas,]$salida <- calls[llamadasAcabadas,]$tiempo_atendida + calls[llamadasAcabadas,]$tiempo_espera + calls[llamadasAcabadas,]$entrada
      
      #Obtengo los operadores a liberar
      idOperadoresLlamadaFinalizada <- calls[llamadasAcabadas,]$idOperador
      idOperadoresLlamadaFinalizada
      
      #libero a los operadoras
      ops[idOperadoresLlamadaFinalizada,]$estado <- "libre"
      ops[idOperadoresLlamadaFinalizada,]$idLlamadaAtendiendo <- 0
    }
    
    
    #Quitamos del pool de llamadas las finalizadas y las almacenamos para el Log
    if(!exists("llamadasAAlmacenar")){
      llamadasAAlmacenar <<- calls[calls$salida!=0,]
    } else {
      llamadasAAlmacenar <<- rbind(llamadasAAlmacenar, calls[calls$salida!=0,])
    }
    calls <- calls[calls$salida==0,]
  }#Fin del IF de si hay llamadas a procesar
  
  operadores <<- ops
  llamadas <<- calls
  
  #Debug
  if(identical(debug,TRUE)){
    print(llamadas)
    print(operadores)
    readline("Press a key to continue...")
  }
  
}