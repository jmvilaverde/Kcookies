#source("colaRecursos.R")

recursos.init <- function(){
  recursos_requeridos <<- getCsv("./Recursos.csv", ";")
  recursos_disponibles <<- getCsv("./Recursos_disponibles.csv", ";")
  
  #Elimina los recursos que queden de anteriores ejecuciones
  #remove("recursos")
  #remove("colaRecursos")
  
  contadorIdRecurso <- 0
  #Creacion de lista de recursos
  for(i in 1:nrow(recursos_disponibles)){
    #Creamos tantos recursos como cantidad del recurso dispongamos
    for(j in 1:recursos_disponibles[i,]$Cantidad){
      
      #Incrementamos el contador de id del recurso
      contadorIdRecurso <- contadorIdRecurso + 1
      capacidad_recurso <- as.integer(recursos_disponibles[i,]$Capacidad)
      recursos_aux <- data.frame(recursos_disponibles[i,]$Recurso, contadorIdRecurso, recursos_disponibles[i,]$Capacidad, 
                                 "Libre", 0, 0, 0, "", "", "", stringsAsFactors=FALSE)
      colnames(recursos_aux) <- c("Recurso", "idRecurso", "Capacidad", 
                                  "Estado", "CiclosEnEstado", "CiclosActivo", "CiclosEnEspera", "pedido", "subpedido", "fase")
      
      if (!exists("recursos")){
        recursos <<- recursos_aux
      }else{
        recursos <<- rbind(recursos, recursos_aux)
      }
      
      #Creamos cola de recursos en los casos que sea necesario
      # if(capacidad_recurso>1 & recursos_disponibles[i,]$crearCola == TRUE) colaRecursos.new(capacidad_recurso, recursos[i,])
    }
    
    
  }
}

recursos.limpiar <- function(){
  recursos2 <<- recursos
  recursos <<- recursos[0,]
}

#Devuelve los recursos que se necesitan para una fase
recursos.getRequeridosParaFase <- function(fase){
  
    return(recursos_requeridos[recursos_requeridos$Fase==fase,]$Recurso)
   
}

recursos.maximoProcesablePorFase <- function(nombreRecursos){
  resultado <- min(recursos[recursos$Recurso == nombreRecursos,]$Capacidad)
  #print(paste("Resultado de recursos.maximoProcesablePorFase:",resultado))
  return(resultado)
}

#Obtiene los recursos disponibles de un recurso para un pedido
#Un recurso está libre para un pedido si está libre o ya está asignado a dicho pedido
recursos.getRecursosPorTipoLibres <- function(recurso, pedido){
  
  listaRecursos <- recursos[recursos$Recurso==recurso,]
  print(paste("Recurso: ", recurso))
  print("Debug recursos.getRecursosPorTipoLibres:")
  print(listaRecursos)
  #Creamos lista vacia de recursos libres
  recursosLibres <- listaRecursos[0,]
  # print("recursosLibres valor inicial")
  # print(recursosLibres)
  
  for(i in 1:nrow(listaRecursos)){
    auxRecursoEnLista <- listaRecursos[i,]
    recursoEstaLibre <- FALSE
    auxIdRecurso <- auxRecursoEnLista$idRecurso
    #Si es un recurso que existe en la cola de recursos comprobamos si la cola está libre o es un pedido que está ya en la cola (Batidora)
    #auxColaRecurso <- colaRecursos[colaRecursos$idRecurso==auxIdRecurso,]
    #dimensionCola <- nrow(auxColaRecurso)
    #if(dimensionCola>0){ #Es un recurso con cola
      
      #Si hay ya un recurso en Colocar_masa no podemos usar ese mismo recurso para uno que pasa de Mezclar a Colocar_masa
      #print(paste("Hay cola. auxIdRecurso:", auxIdRecurso, sep=" "))
      fase_pedido <- pedidos[pedidos$idSubpedido == pedido,]$fase
      print(paste("Fase pedido:",fase_pedido, sep=" "))
      print(paste("Pedido: ", pedido))
      
      #print(paste(colaRecursos.existePedidoEnCola(auxColaRecurso, pedido), colaRecursos.estaVacia(auxColaRecurso), sep=" "))
      #Si ((no existe ningún pedido asignado al recurso y que este en la siguiente fase o la cola está vacía
      #if((!colaRecursos.existePedidoEnColaEnFase(auxIdRecurso, fase_pedido) && colaRecursos.existePedidoEnCola(auxColaRecurso, pedido)) | colaRecursos.estaVacia(auxColaRecurso)) recursoEstaLibre <- TRUE
      #Si el pedido está en la cola o la cola está vacía entonces el recurso está libre para ese pedido
      # if(colaRecursos.existePedidoEnCola(auxColaRecurso, pedido) | colaRecursos.estaVacia(auxColaRecurso)) recursoEstaLibre <- TRUE
    #} else { #Es un recurso sin cola
      #Si está libre o esta asignado al pedido entonces el recurso está libre para el pedido
      print(auxRecursoEnLista)
      if(auxRecursoEnLista$Estado == "Libre" | auxRecursoEnLista$subpedido == pedido) recursoEstaLibre <- TRUE
    #} 
    if(recursoEstaLibre) recursosLibres <- rbind(recursosLibres, auxRecursoEnLista)
  }
  
  # print("recursosLibres valor final")
  # print(recursosLibres)
  
  # print("colaRecursos:")
  # print(colaRecursos)
  
  return(recursosLibres)
}

#Verifica que tengamos disponibles todos los recursos necesarios para la fase
#Si nos falta algún recurso disponible entonces devolvemos falso
recursos.checkDisponibles <- function(fase, pedido){
  
  #Si va a pasar a la fase de Cobrar comprobamos primero que sea ya la hora de cobrar el pedido
  print(paste("fase: ", fase,", hora_recogida: ",pedidos[pedido,]$hora_recogida, ", hora_actual:", minutos_to_horas(hora_inicio+paso)))
  if(fase == "Cobrar" & pedidos[pedido,]$hora_recogida > minutos_to_horas(hora_inicio+paso)) return(FALSE)
  
  #Si está en la fase Final directamente devolvemos que hay recursos para que termine
  if(fase == "Final") return(TRUE)
  
  recursos_necesarios_fase <- recursos.getRequeridosParaFase(fase)
  # print("--------------------------------------------------------------------------------------------")
  # print(paste("Debug recursos.checkDisponibles - comprobacion disponibles:",fase,pedido, sep=" "))
  #print(paste("Recursos_necesarios_fase", length(recursos_necesarios_fase), sep=""))
  for(i in 1:length(recursos_necesarios_fase)){
    recursos_libres <- recursos.getRecursosPorTipoLibres(recursos_necesarios_fase[i], pedido)
    #Si no hay recursos libres de algún tipo te devuelve FALSE
    # print(paste("Numero filas de recurso",recursos_necesarios_fase[i],":",nrow(recursos_libres),sep=" "))
    if(nrow(recursos_libres)==0) return(FALSE)
  }
  return(TRUE)

}

#Funcion para liberar los recursos ocupados por un subpedido
recursos.liberar <- function(pedido){
  #Comprobamos si el subpedido tiene recursos ocupados
  if(nrow(recursos[recursos$subpedido == pedido$idSubpedido,])>0){
    recursos[recursos$subpedido == pedido$idSubpedido,]$Estado <- "Libre"
    recursos[recursos$subpedido == pedido$idSubpedido,]$pedido <- 0
    recursos[recursos$subpedido == pedido$idSubpedido,]$CiclosEnEstado <- 0
    recursos[recursos$subpedido == pedido$idSubpedido,]$fase <- ""
    #Importante cambiar el valor de subpedido lo último, ya que nos sirve de índice
    recursos[recursos$subpedido == pedido$idSubpedido,]$subpedido <- 0
    #Limpiamos colas del recurso
    #colaRecursos.popPedido(pedido)
  }
  return(recursos)
}

recursos.liberarPersonas <- function(pedido){
  print("Pedido en liberarPersonas")
  print(pedido)
  recursosPersonasdelPedido <- recursos$subpedido == pedido$idSubpedido & recursos$Recurso == "Persona"
  #Comprobamos si el subpedido tiene recursos ocupados
  if(nrow(recursos[recursosPersonasdelPedido,])>0){
    recursos[recursosPersonasdelPedido,]$Estado <- "Libre"
    recursos[recursosPersonasdelPedido,]$pedido <- 0
    recursos[recursosPersonasdelPedido,]$CiclosEnEstado <- 0
    recursos[recursosPersonasdelPedido,]$fase <- ""
    #Importante cambiar el valor de subpedido lo último, ya que nos sirve de índice
    recursos[recursosPersonasdelPedido,]$subpedido <- 0
  }
  return(recursos)
}

###############
# Importante, una persona es un recurso que se puede liberar de una fase a otra, esto se puede cambiar en el excel
###############
recursos.ocupar <- function(pedido){
  fase <- pedido$fase
  fase_anterior <- fases.getFaseAnterior(fase)$Fase
  recursos_necesarios_fase <- recursos.getRequeridosParaFase(fase)
  
  #Obtenemos el máximo de docenas de cookies procesables en la fase
  # !!! Desactivamos de momento el máximo procesable
  #maximoProcesable <- recursos.maximoProcesablePorFase(recursos_necesarios_fase)
  
  #Obtenemos el listado de los recursos que necesitamos en la fase
  for(i in 1:length(recursos_necesarios_fase)){
    recurso_necesario <<- recursos_necesarios_fase[i]
    
    #Buscamos el primer recurso libre
    encontrado <- FALSE

    #Recorremos el listado de recursos
    for(j in 1:nrow(recursos)){
      #Vemos si el recurso es el que necesitamos, y si está libre o está asignado a un recurso nuestro, y no se ha encontrado un recurso
      if (recursos[j,]$Recurso == recurso_necesario & (recursos[j,]$Estado=="Libre" | recursos[j,]$idRecurso == pedido$idRecurso) & !encontrado){
        #Ocupamos el recurso
        encontrado <- TRUE
        recursos[j,]$Estado <- "Ocupado"
        recursos[j,]$pedido <- pedido$idPedido
        recursos[j,]$subpedido <- pedido$idSubpedido
        recursos[j,]$fase <- fase
        
        #Obtenemos la cola del recurso
        idRecurso <- recursos[j,]$idRecurso
        #aux_colaRecursos <- colaRecursos.getColaPorRecurso(idRecurso)
        
        # !!! Desactivamos de momento el máximo procesable
        #if(colaRecursos.existeColaPorRecurso(idRecurso)){
        #if(colaRecursos.existeColaPorRecurso(idRecurso) & maximoProcesable > 1){
          #permiteMezclarSabores <- recursos_disponibles[recursos_disponibles$Recurso == recursos[j,]$Recurso,]$permiteMezclarSabores
          #Buscamos los subpedidos del pedido que tengan que pasar a la misma fase que estamos gestionando en el pedido actual
          #aux_subpedidos <- pedidos[pedidos$idPedido == pedido$idPedido & pedidos$fase == fase_anterior,]
          #print(aux_subpedidos)
        #   if(nrow(aux_subpedidos)==0){
        #     aux_subpedidos <- pedido
        #   } else {
        #     aux_subpedidos <- rbind(pedido,aux_subpedidos)
        #   }
        #   print(aux_subpedidos)
        #   #Recorremos los subpedidos y los vamos encolando hasta que se acabe el espacio de la cola del recurso
        #   pedidosEncolados <- 0
        #   indiceAuxColaRecursos <- 1
        #   #
        #   capacidadCola <- colaRecursos.getCapacidadColaRecurso(idRecurso)
        #   for(k in 1:nrow(aux_subpedidos)){
        #     #Solo gestionamos en cola mientras no se haya llenado la cola
        #     if(pedidosEncolados <= capacidadCola & indiceAuxColaRecursos <= capacidadCola){
        #       indiceColaRecursos <- aux_colaRecursos[indiceAuxColaRecursos,]$id
        #       indiceAux_Subpedido <- aux_subpedidos[k,]$idSubpedido
        #       sabor <<- as.integer(aux_subpedidos[k,]$sabor)
        #       if(k==1) sabor_base <<- sabor #Este es el único sabor que podemos meter en la cola
        #       #Encolamos si se pueden mezclar sabores o (no se permite mezclar sabores pero tiene el mismo sabor que el sabor base)
        #       if(permiteMezclarSabores==TRUE | (permiteMezclarSabores == FALSE & sabor == sabor_base)){
        #         colaRecursos.pushPedido(indiceAux_Subpedido, indiceColaRecursos)
        #         pedidos.setFaseParaPedido(indiceAux_Subpedido, fase)
        #         pedidosEncolados <- pedidosEncolados + 1
        #         indiceAuxColaRecursos <- indiceAuxColaRecursos + 1
        #       } 
        #     } 
        #   } #end for k
        #   
        # }
        
      }
    }#end for j
  }#end for i
  return(recursos)
}


#Incrementamos los ciclos de los recursos según su estado
recursos.incrementarCiclos <- function(){
  #Incrementamos el tiempo de espera de los recursos libres
  recursos[recursos$Estado == "Libre",]$CiclosEnEspera <- recursos[recursos$Estado == "Libre",]$CiclosEnEspera + 1
  
  #Incrementamos los ciclos de los ocupados
  recursosOcupados <- recursos[recursos$Estado == "Ocupado",]
  if(nrow(recursosOcupados)>0){
    for (i in 1:nrow(recursosOcupados)){
      auxRecursos <- recursosOcupados[i,]
      tiempoProcesoFase <- flujo_estados[flujo_estados$Fase==auxRecursos$fase,]$Tiempo_proceso
      if(auxRecursos$CiclosEnEstado < tiempoProcesoFase){
        auxRecursos$CiclosActivo <- auxRecursos$CiclosActivo + 1  
      } else {
        auxRecursos$CiclosEnEspera <- auxRecursos$CiclosEnEspera + 1
      }
      auxRecursos$CiclosEnEstado <- auxRecursos$CiclosEnEstado + 1
      recursos[recursosOcupados[i,]$idRecurso,] <- auxRecursos
    }  
  }

  return(recursos)
}

#Guardar el historico del estado de pedidos en cada momento
recursos.guardarHistorico <- function(marcaTiempo, simulacion = 1){
  #Para la ejecución de la función si no hay recursos
  if(nrow(recursos)==0) return()
  recursos_aux <- recursos
  recursos_aux$marcaTiempo <- marcaTiempo
  recursos_aux$simulacion <- simulacion
  
  if(!exists("historicoRecursos")){
    historicoRecursos <<- recursos_aux
  } else {
    historicoRecursos <<- rbind(historicoRecursos, recursos_aux)
  }
}