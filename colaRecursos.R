#Creacion de nueva cola de recursos
#colaRecursos.new <- function(capacidad_recurso, recurso){
#   if(capacidad_recurso>1 & (recurso$Recurso!="Persona")){
#     for(j in 1:capacidad_recurso){
#       colaRecursos_aux <- data.frame(0, recurso$idRecurso, 0, 0, stringsAsFactors = FALSE)
#       colnames(colaRecursos_aux) <- c("id", "idRecurso", "pedido", "subpedido")
#       
#       if(!exists("colaRecursos")){
#         colaRecursos_aux$id <- 1
#         colaRecursos <<- colaRecursos_aux
#       } else {
#         colaRecursos_aux$id <- max(colaRecursos$id)+1
#         colaRecursos <<- rbind(colaRecursos, colaRecursos_aux)
#       }
#     }
#   }
# }

#Devuelve la cola de una recurso
#colaRecursos.getColaPorRecurso <- function(idRecurso){
#   return(colaRecursos[colaRecursos$idRecurso==idRecurso,])
# }

#Devuelve un boolean indicando si existe o no la cola para el recurso
#colaRecursos.existeColaPorRecurso <- function(idRecurso){
#   auxColaRecursos <- colaRecursos.getColaPorRecurso(idRecurso)
#   if(nrow(auxColaRecursos)>0) {
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
# }

#Devuelve un boolean si existe el pedido en la cola
#colaRecursos.existePedidoEnCola <- function(colaRecurso,subpedido){
#   result <- ifelse(nrow(colaRecurso[colaRecurso$subpedido==subpedido,])>0,TRUE,FALSE)
#   return(result)
# }

#Devuelve un boolean indicando si está la cola vacia
#colaRecursos.estaVacia <- function(auxColaRecurso){
#   dimensionCola <- nrow(auxColaRecurso)
#   espaciosCola <- nrow(auxColaRecurso[auxColaRecurso$subpedido==0,])
#   print(paste("Espacios de la cola: ", espaciosCola, "/", dimensionCola))
#   print(auxColaRecurso)
#   result <- ifelse(dimensionCola==espaciosCola, TRUE, FALSE)
#   return(result)
# }

#Devuelve TRUE si existe algún pedido en la fase para el recurso 
#colaRecursos.existePedidoEnColaEnFase <- function(idRecurso, fase){
#   auxPedidos <- pedidos[pedidos$fase==fase,]$idSubpedido
#   print(auxPedidos)
#   numPedidos <- nrow(colaRecursos[colaRecursos$subpedido==auxPedidos & colaRecursos$idRecurso==idRecurso,])
#   ifelse (numPedidos>0, return(TRUE), return(FALSE))
# }

#Devuelve la capacidad de la cola del recurso
#colaRecursos.getCapacidadColaRecurso <- function(idRecurso){
  return(nrow(colaRecursos.getColaPorRecurso(idRecurso)))
}

#Push en la cola
#colaRecursos.pushPedido <- function(indiceSubpedido, indiceColaRecursos){
  subpedido <- pedidos[indiceSubpedido,]
  colaRecursos[indiceColaRecursos,]$pedido <<- subpedido$idPedido
  colaRecursos[indiceColaRecursos,]$subpedido <<- subpedido$idSubpedido
  pedidos.setValoresColaEnPedido(indiceSubpedido, indiceColaRecursos)
}

#Pop de la cola
#colaRecursos.popPedido <- function(pedido){
  #print(paste("Pop de la cola para pedido", pedido$idColaRecurso))
  indiceColaRecursos <- pedido$idColaRecurso
  colaRecursos[indiceColaRecursos,]$pedido <<- 0
  colaRecursos[indiceColaRecursos,]$subpedido <<- 0
}