random_sabor <- function(sabores){
  return(sabores[as.integer(runif(1,0,length(sabores))) + 1])
}

pedido.randomGeneratorPedidos <- function(hora_pedido){
  
  #Dejamos para más adelante el cambiar la función random según la franja horaria
  #numeroPedidos <- pedido.getRandomNumeroPedidos(hora = hora_pedido, pedidos_por_turno = 10, minutos_por_turno = 240)
  
  #Dejamos fijo el número de pedidos a 1, si hay menos de 2 pedidos en el listado
  numeroPedidos <- pedido.getRandomNumeroPedidos(pedidos_por_turno = 10, minutos_por_turno = contador.getMinutosTurno())
  # if (!exists("pedidos")) {
  #   numeroPedidos <- 1  
  # }
  # else {
  #   if(nrow(pedidos)<2){
  #     numeroPedidos <- 1
  #   } else {
  #     numeroPedidos <- 0
  #   }
  # }
  
  if(numeroPedidos>0){
    for(i in 1:numeroPedidos){
      pedido_aux <- pedido.new(hora=hora_pedido, p_tamano=c(0.15, 0.30, 0.55), p_iguales=1, sabores = c(1:4))
      ifelse(!exists("pedidos"), pedidos <<- pedido_aux, pedidos <<- pedido.add(pedidos, pedido_aux))
    }
  }
  return(numeroPedidos)
}


pedido.getRandomNumeroPedidos <- function(pedidos_por_turno = 20, minutos_por_turno = 240){

  media <- pedidos_por_turno/minutos_por_turno
  numeroPedidos <- rpois(1,media)
  return(numeroPedidos)
}


pedido.add <- function(pedidos, pedido){
  pedidos <- rbind.data.frame(pedidos, pedido)
  return(pedidos)
}

#p_tamaño es la probabilidad de que un pedido sea de 1,2 o 3 docenas #Fijado a 1 docena
#p_iguales es la probabilidad de que un pedido sea del mismo sabor
pedido.new <- function(hora, p_tamano=c(0.15, 0.30, 1), p_iguales=1.0, sabores = c(1:4)){
  #Veremos si esto puede ser variable
  minutosParaRecoger <- 30
  
  #De momento dejamos fija la cantidad a 1
  #TO-DO: Gestión de pedido con más de una docena y varios sabores
  ran = runif(1, 0, 100)/100
  cantidad <- 1
  # if(ran <= p_tamano[3]) {
  #   cantidad <- 1
  # }
  # else if (ran <= sum(p_tamano[2:3])) {
  #   cantidad <- 2
  # } else {
  #     cantidad <- 3
  #   }

  if(!exists("contadorIdPedido")){
    contadorIdPedido <<- 1
  } else {
    contadorIdPedido <<- contadorIdPedido+1
  }
  
  sabor <- random_sabor(sabores)
  mismo_sabor <- runif(1, 0, 100)/100 > p_iguales
  
  hora_entrada <- minutos_to_horas(hora_inicio + horas_to_minutos(hora))
  hora_recogida <- minutos_to_horas(hora_inicio+horas_to_minutos(hora)+minutosParaRecoger)
  
  for(i in 1:cantidad){
    if (mismo_sabor) sabor <- random_sabor(sabores)
    if(!exists("contadorIdSubPedido")){contadorIdSubPedido <<- 1} else {contadorIdSubPedido <<- contadorIdSubPedido+1}
    
    pedido_aux <- data.frame(contadorIdPedido, contadorIdSubPedido, sabor, "Inicial", 0, 0, 0, 0, 0, 0, hora_entrada, hora_recogida, "", stringsAsFactors=FALSE)
    
    if(!exists("pedido")) pedido <- pedido_aux else pedido <- pedido.add(pedido, pedido_aux)
  }
  colnames(pedido) <- c("idPedido", "idSubpedido", "sabor", "fase", "idRecurso", "idColaRecurso", "tiempo_procesamiento_fase", "tiempo_espera", "tiempo_almacen", "tiempo_atendido", "entrada", "hora_recogida", "salida")
  return(pedido)
}

#Limpiamos el listado de pedidos
pedidos.limpiar <- function(){
  #remove("pedidos")
  pedidos2 <<- pedidos
  pedidos <<- pedidos[0,]
  contadorIdPedido <<- 0
  contadorIdSubPedido <<- 0
}

pedidos.setValoresColaEnPedido <- function(idSubpedido, idColaRecursos){
  auxColaRecursos <- colaRecursos[idColaRecursos,]
  print(paste("pedidos.setValoresColaEnPedido:",idSubpedido))
  pedidos[idSubpedido,]$idRecurso <<- auxColaRecursos$idRecurso
  pedidos[idSubpedido,]$idColaRecurso <<- auxColaRecursos$id
}

#Cambia de fase un pedido y pone a 0 el contador de tiempo de procesamiento en la fase
pedidos.setFaseParaPedido <- function(indiceAux_Subpedido, fase){
  index <- pedidos$idPedido==indiceAux_Subpedido
  print(paste("Indice setFaseParaPedido:", index, sep=" "))
  auxPedido <- pedidos[index,]
  auxPedido$fase <- fase
  auxPedido$tiempo_procesamiento_fase <- 0
  
  #20170426 Control de hora de salida cuando la fase es final
  if(fase=="Final") auxPedido$salida <- minutos_to_horas(hora_inicio+paso)
  
#   auxPedido$idRecurso <- 0
#   auxPedido$idColaRecurso <- 0
  pedidos[index,] <<- auxPedido
}

pedidos.quitarPedidoDeCola <- function(idSubpedido){
  pedidos[idSubpedido,]$idRecurso <<- 0
  pedidos[idSubpedido,]$idColaRecurso <<- 0
}


#Incrementar el paso de los pedidos teniendo en cuenta si están ocupados o no
pedidos.incrementarPaso <- function(){
  
  for(i in 1:nrow(pedidos)){
    pedido <- pedidos[i,]
    fase <- flujo_estados[flujo_estados$Fase == pedido$fase,]
    #Si se ha superado el tiempo de procesamiento de la fase lo metemos en tiempo_espera
    if(pedido$tiempo_procesamiento_fase >= fase$Tiempo_proceso){
      pedido$tiempo_espera <- pedido$tiempo_espera + 1
    } else {
        pedido$tiempo_procesamiento_fase <- pedido$tiempo_procesamiento_fase + 1
      if(fase$Tipo != "Almacen"){ 
        pedido$tiempo_atendido <- pedido$tiempo_atendido + 1
      }
    }
    
    #Si está en un almacen incrementamos el tiempo en almacen
    if(fase$Tipo == "Almacen"){ pedido$tiempo_almacen <- pedido$tiempo_almacen + 1 }    
    pedidos[i,] <- pedido
  }
  
  return(pedidos)
  
}

#Guardar el historico del estado de pedidos en cada momento
pedidos.guardarHistorico <- function(marcaTiempo, simulacion = 1){
  pedidos_aux <- pedidos
  pedidos_aux$marcaTiempo <- marcaTiempo
  pedidos_aux$simulacion <- simulacion
  
  if(!exists("historicoPedidos")){
    historicoPedidos <<- pedidos_aux
  } else {
    historicoPedidos <<- rbind(historicoPedidos, pedidos_aux)
  }
}

#Funcion para retirar los pedidos finalizados
pedidos.retirarFinalizados <- function(marcaTiempo){
  return(pedidos[pedidos$fase!="Final", ])
}