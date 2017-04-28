


#Generador

#Controlador

#Llamadas

get.Ramdon.Dept <- function(){
  ran = runif(1, 0, 100)
  tipo <- "0"
  
  for (i in 1:nrow(global.departamentos)){
    if(global.departamentos[i,]$Probabilidad >= ran && tipo == "0"){
      tipo <- as.character(global.departamentos[i,]$Departamento)
    }
  }

  return(tipo)
}

get.Section <- function(dept){
  
    #convierte el departamento en seccion
  global.departamentos %>% filter(Departamento == dept) %>% select(Seccion) %>% as.character()-> seccion
  
  if(length(seccion)==0) seccion <- "0"
  
  return(seccion)
  
}

new.Llamada <- function(hora){
  ran = runif(1, 0, 100)/100
  if(ran < 0.1) {
    tipo <- "corto"
    duracion <- 50
  }
  else if (ran < 0.8) {
    tipo <- "media"
    duracion <- 90
  } else {
      tipo <- "larga"
      duracion <- 300
  }
  
  dept <- get.Ramdon.Dept()
  seccion <- get.Section(dept)
  
  if(!exists("llamadas")){
    id <- 1
  } else {
    id <- nrow(llamadas)+1
  }
  
  llamada <- data.frame(id, dept, seccion, tipo, duracion, FALSE, 0, 0, Constante_tiempo_limite_espera, hora, 0, 0, stringsAsFactors=FALSE)
  colnames(llamada) <- c("id", "departamento","seccion", "tipo", "duracion", "atendida", "tiempo_espera", "tiempo_atendida", "limite_espera", "entrada", "salida", "idOperador")
  return(llamada)
}

add.Llamada <- function(listaLlamadas, llamada){
  lista <- rbind.data.frame(listaLlamadas, llamada)
  return(lista)
}

# get.Llamada <- function(){
#   llamada <- data.frame("corta", 50, FALSE, 0, Constante_tiempo_limite_espera)
#   colnames(llamada) <- c("tipo", "duracion", "atendida", "tiempo espera", "limite espera")
#   return(llamada)
# }

get.NumeroLlamadas <- function(min=0, max=5){
  numeroLlamadas <- round(runif(1, min=min, max=max),0)
  return(numeroLlamadas)
}


#Directorio

#Agentes

new.Operador <- function(id = 0, departamento="A", horario=1){
  operador <- data.frame(id, 0, departamento, horario, TRUE, "libre", 0, 0, 0, stringsAsFactors=FALSE)
  colnames(operador) <- c("id", "seccion", "departamento", "horario", "enHorarioDeTrabajo", "estado", "idLlamadaAtendiendo", "tiempoOcioso", "tiempoAtendiendo") 
  return(operador)
}

add.Operadores <- function(listaOperadores, operador){
  lista <- rbind.data.frame(listaOperadores, operador)
  return(lista)
}

create.CallCenter <- function(){

  ruta <- paste("./configOperadoresFase",Constante.FaseSimulacion,".csv", sep = "")
  operadores <<- getCsv(ruta = ruta)
  
  if(nrow(operadores)==0){
    operadores <- new.Operador(id = 1, departamento = "A", horario = 1)
    
    #Asignación
    for (i in 2:numero.Operadores){
      nuevoOperador <- new.Operador(id = i, departamento = global.departamentos[i%%length(global.departamentos)+1], horario = 1)
      operadores <- add.Operadores(operadores, nuevoOperador)
    }  
  }
  
  operadores$estado <<- as.character(operadores$estado)
  return(operadores)
}

controlHorarioTrabajo <- function(ops){
  #situacionHoraria
  
  if(nrow(global.horarios)>0){
    for(i in 1:nrow(global.horarios)){
      #va de media hora en media hora, eso son 30x60 = 1800 el salto es cada 1800, necesitamos la parte entera de dividir
      #situacionHoraria entre 1800 franja horaria
      hora <- as.integer(situacionHoraria/Constante.FranjaHoraria)+2
      
    #TODO  
    ops[ops$horario==global.horarios[i,1],]$enHorarioDeTrabajo <- global.horarios[i,hora]
    
    return(ops)
  }
}
  
}


#Control

add.Step <- function(debug=FALSE){
  
  library(dplyr)
  
  ops <- operadores
  calls <- llamadas

  #Comprobación de horario de trabajo
  ops <- controlHorarioTrabajo(ops)
  
  
  #Si hay operadores libres asignamos llamadas
  filter(operadores, estado=="libre") %>% nrow() %>% as.integer() -> operadoresLibres

  if(operadoresLibres>0){
  
    #Asigna llamadas pendientes a los operadores en estado libre
    for(i in 1:nrow(calls)){
      idOperadorLibre <- 0
      
      if (identical(calls[i,]$atendida,FALSE)){
        dept <- calls[i,]$departamento
        
        #Mira si el departamento requerido tiene operadores disponibles y tomamos el que tiene menos ocupación
        
          filter(ops, departamento == as.character(dept), estado == "libre") %>% 
            arrange(desc(tiempoAtendiendo)) %>% select(id) %>% head(1) -> idOperadorTemp
          
          
          
          if (nrow(idOperadorTemp) == 1) idOperadorLibre <- as.integer(idOperadorTemp[1])
    
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
            ops[idOperadorLibre,]$estado <- Constante.Ocupado
            ops[idOperadorLibre,]$idLlamadaAtendiendo <- i
            calls[i,]$atendida <- TRUE
            calls[i,]$idOperador <- idOperadorLibre
            
          }
      }
      
    }#Fin del For
  }

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
  
  #Incremento los tiempos de las llamadas
  
  for(i in 1:nrow(calls)){  
  
    #Incremento el tiempo de espera de las llamadas que no han sido atendidas
    if(identical(calls[i,]$atendida,FALSE) && calls[i,]$salida == 0){
      calls[i,]$tiempo_espera <- calls[i,]$tiempo_espera + paso
      #Si se ha superado el tiempo de espera se saca del sistema
      if(calls[i,]$tiempo_espera ==  calls[i,]$limite_espera){
        calls[i,]$salida <- calls[i,]$entrada + calls[i,]$tiempo_espera
      }
    }
  
    #Incremento el tiempo de las llamadas que están siendo atendidas
    if(identical(calls[i,]$atendida,TRUE) && calls[i,]$salida == 0){
      #incrementa tiempo de llamada
      calls[i,]$tiempo_atendida <- calls[i,]$tiempo_atendida + paso
      idOperador <- as.integer(calls[i,]$idOperador)
      ops[idOperador,]$tiempoAtendiendo <- ops[idOperador,]$tiempoAtendiendo + paso
      
      if(calls[i,]$tiempo_atendida >= calls[i,]$duracion){
        calls[i,]$salida <- calls[i,]$tiempo_atendida + calls[i,]$tiempo_espera + calls[i,]$entrada
        
        #si ha terminado libera al operador
        ops[idOperador,]$estado = "libre"
        ops[idOperador,]$idLlamadaAtendiendo = 0
      }
          
    }

  }#Fin del for de llamadas  

  operadores <<- ops
  llamadas <<- calls
  
  #Debug
  if(identical(debug,TRUE)){
    print(llamadas)
    print(operadores)
    readline("Press a key to continue...")
  }
   
  
}

log.Todo <- function(timemark){
  log(llamadas, "llamadas",timemark)
  log(operadores, "operadores",timemark)
  log(StepByStep.llamadas, "StepLlamadas", timemark)
  log(StepByStep.operadores, "StepOperadores",timemark)
}

log <- function(obj, nombre, timemark){
  ruta = paste("./log",nombre,timemark,".csv",sep="")
  write.csv(obj, file=ruta, sep=";")
}

stats <- function(){
  
  stats <- llamadas
  hist(llamadas$entrada)
  
}

getCsv <- function(ruta){
  df = read.csv(ruta, sep=";")
  return(df)
}

loadConstants <- function(){
  
  rutaDepartamentos <- paste("./configDepartamentosFase",Constante.FaseSimulacion,".csv",sep ="")
  
  departamentos <- getCsv(rutaDepartamentos)
  departamentos %>% #select(Departamento, Probabilidad) %>% 
    mutate(Probabilidad = as.character(Probabilidad)) %>% 
    mutate(Probabilidad = as.numeric(sub(",",".",Probabilidad))) ->> global.departamentos
  
  if(length(global.departamentos)>1){
    for (i in 2:nrow(global.departamentos)){
      #Asigna probabilidad
      global.departamentos[i,]$Probabilidad <- global.departamentos[i,]$Probabilidad + global.departamentos[i-1,]$Probabilidad
    }  
  }
  
  
  global.horarios <<- getCsv("./configHorarios.csv")
  Constante.FranjaHoraria <<-1800 #1800 segundos = 30 minutos cada franja
  
  
  if(Constante.FaseSimulacion==1){
    envioOtrasSecciones <<- TRUE
  } else if(Constante.FaseSimulacion==2){
    envioOtrasSecciones <<- FALSE
  }
  
  
}

main <- function(fase = 1, timemark="0", debug=FALSE){
  Constante_tiempo_limite_espera <<- 60
  Constante_ocupacion_maxima <<- 0.8
  Constante.Ocupado <<- "ocupado"
  Constante.Libre <<- "libre"
  Constante.FaseSimulacion <<- fase
  
  loadConstants()
  
  #global.departamentos <<- c("A", "B", "C")
  #numero.Operadores <<- 11
  
  operadores <<- create.CallCenter()
  
  start <- 0
  #end <- 100
  end <- 60*8
  paso <<- 10
  #index <- 0
  esPrimeraLlamada <- TRUE
  
  print(timestamp())
  
  #Control de proceso
  for(i in seq(from=start, to=end, by=paso)){
    situacionHoraria <<- i
    #Obtenemos el número de llamadas que entran en el intervalo
    numeroLlamadas <- get.NumeroLlamadas()
    #Si es mayor que 0 se gestiona
    if (numeroLlamadas!=0){
      #Gestionamos cada llamada
      for(j in 1:numeroLlamadas){
        #Generamos una llamada
        newLlamada <- new.Llamada(hora=i)
        #Si no es la primera llamada lo añadimos a la lista
        if (!esPrimeraLlamada){
          llamadas <<- add.Llamada(llamadas, newLlamada)
        } else { 
          #Si es la primera llamada creamos la lista con el primer elemento
          llamadas <<- newLlamada
          esPrimeraLlamada <- FALSE
        }
        
        
      }
    }

    #Iteramos sobre los operadores para ver quien ha terminado
    #TODO Continuar trabajando desde aquí
    #Hacer llamadas y operadores variables globales
    #Ponemos el if para que no empiece a iterar hasta que hayan llamadas
    if (nrow(llamadas)>0)  add.Step(debug)
    
    llamadas %>% mutate(Paso=i) -> OneStep.llamadas
    operadores %>% mutate(Paso=i) -> OneStep.operadores
    
    if(i==start){
      StepByStep.llamadas <<- OneStep.llamadas
      StepByStep.operadores <<- OneStep.operadores
      
    } else {
      StepByStep.llamadas <<- rbind(StepByStep.llamadas, OneStep.llamadas)
      StepByStep.operadores <<- rbind(StepByStep.operadores, OneStep.operadores)
      
    }
    
  }
  
  print(timestamp())
  #Guardamos un Log con todo el proceso
  log.Todo(timemark)

}