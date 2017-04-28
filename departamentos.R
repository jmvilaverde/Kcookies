#########
#Horarios
#########

Horarios.getHorariosFromCSV <- function(ruta){
  horarios <<- getCsv(ruta)
  horarios[,-1] <- (horarios[,-1]==1)
  horarios <- horarios[!is.na(horarios$Tipo),]
  return(horarios)
}

##############
#Departamento
##############

Departamento.getRandomDepartamento <- function(){
 
  ran = runif(1, 0, 100)
  tipo <- "0"
  
  for (i in 1:nrow(global.departamentos)){
    if(global.departamentos[i,]$Probabilidad >= ran && tipo == "0"){
      tipo <- as.character(global.departamentos[i,]$Departamento)
    }
  }

  return(tipo)
}

#Obtener los departamentos del csv de configuracion
Departamento.getDepartamentosFromCSV <- function(rutaCSV){
 
  aux <- getCsv(rutaCSV)
  aux %>% mutate(Probabilidad = as.character(Probabilidad)) %>% 
    mutate(Probabilidad = as.numeric(sub(",",".",Probabilidad))) -> departamentos
  
  departamentos <- departamentos[!is.na(departamentos$Seccion),]
  
  if(length(departamentos)>1){
    for (i in 2:nrow(departamentos)){
      #Asigna probabilidad
      departamentos[i,]$Probabilidad <- departamentos[i,]$Probabilidad + departamentos[i-1,]$Probabilidad
    }  
  }
  
  
  return(departamentos)
}


########
#Operadores
########

# Crea nuevo operador
# Uso en modo debug para creacion de usuarios aleatorios
Operador.new <- function(id = 0, departamento="A", horario=1){
  operador <- data.frame(id, 0, departamento, horario, TRUE, "libre", 0, 0, 0, stringsAsFactors=FALSE)
  colnames(operador) <- c("id", "seccion", "departamento", "horario", "enHorarioDeTrabajo", "estado", "idLlamadaAtendiendo", "tiempoOcioso", "tiempoAtendiendo") 
  return(operador)
}

# Añade operador a lista de operadores
# Uso en modo debug para creacion de usuarios aleatorios
Operador.addToListaOperadores <- function(listaOperadores, operador){
  lista <- rbind.data.frame(listaOperadores, operador)
  return(lista)
}

Operador.createCallCenter <- function(ruta){
  
  operadores <<- getCsv(ruta = ruta)
  
  # if(nrow(operadores)==0){
  #   operadores <- Operador.new(id = 1, departamento = "A", horario = 1)
  #   
  #   #Asignación
  #   for (i in 2:numero.Operadores){
  #     nuevoOperador <- Operador.new(id = i, departamento = global.departamentos[i%%length(global.departamentos)+1], horario = 1)
  #     operadores <- Operador.addToListaOperadores(operadores, nuevoOperador)
  #   }  
  # }
  
  #operadores$estado <<- as.character(operadores$estado)
  operadores %>% mutate(seccion = as.integer(seccion), estado = as.character(estado), enHorarioDeTrabajo=(enHorarioDeTrabajo==1)) -> operadores
  return(operadores)
}

Operador.controlHorarioTrabajo <- function(ops, horarios){
  #situacionHoraria
  
  if(nrow(horarios)>0){
    for(i in 1:nrow(horarios)){
      #va de media hora en media hora, eso son 30x60 = 1800 el salto es cada 1800, necesitamos la parte entera de dividir
      #situacionHoraria entre 1800 franja horaria
      hora <- as.integer(situacionHoraria/Constante.FranjaHoraria)+2
      
      if(nrow(ops[ops$horario==as.integer(horarios[i,1]),])>0){
        ops[ops$horario==as.integer(horarios[i,1]),]$enHorarioDeTrabajo <- as.logical(horarios[i,hora])  
      }
    }
  }
  
  return(ops)
}

Operador.getNumeroOperadorLibres <- function(ops){
  filter(ops, estado=="libre", enHorarioDeTrabajo==TRUE) %>% nrow() %>% as.integer() -> numero
  return(numero)
}


Operador.getOperadorLibreByDepartamento <- function(ops, dept){
  
  idOperadorLibre <- 0
  
  filter(ops, departamento == as.character(dept), estado == "libre", enHorarioDeTrabajo==TRUE) %>% 
    arrange(tiempoAtendiendo) %>% select(id) %>% head(1) -> idOperadorTemp
  
  if (nrow(idOperadorTemp) == 1) idOperadorLibre <- as.integer(idOperadorTemp[1])
  
  return(idOperadorLibre)
}

#Elegimos el operador libre de la seccion que tenga menos tiempo atendiendo
Operador.getOperadorLibreBySeccion <- function(ops, sec){
  
  idOperadorLibre <- 0
  
  filter(ops, seccion == as.integer(sec), estado == "libre", enHorarioDeTrabajo==TRUE) %>% 
    arrange(tiempoAtendiendo) %>% select(id) %>% head(1) -> idOperadorTemp
  
  if (nrow(idOperadorTemp) == 1) idOperadorLibre <- as.integer(idOperadorTemp[1])
  
  return(idOperadorLibre)
}