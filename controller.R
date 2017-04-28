source("auxiliar.R")
source("contador.R")
source("fases.R")
source("recursos.R")
source("pedidos.R")

#Inicialización del controlador
#Inicializamos recursos, fases y el contador #Variables globales
controller.init <- function(h_inicio, h_fin){
  fases.init()
  contador.init(inicio=h_inicio, fin=h_fin, paso_minuto = 1)
  
}

controller.ejecuta_paso <- function(paso, idSimulacion, debug = FALSE){
  
  #Inicializo la variable global de paso
  paso <<- paso
  
  #Genera pedidos en el paso y agrega a la lista de pedidos
  pedido.randomGeneratorPedidos(minutos_to_horas(paso))
  
  #Coje siguiente pedido de la lista de pedidos
  if(exists("pedidos")){
    if(nrow(pedidos) >0){
      
      #Recorremos todas las fases en sentido inverso
      #Liberamos el final de la cadena de producción para que puedan avanzar las etapas
      for(n in nrow(flujo_estados):1){
        fase_a_revisar <- flujo_estados[n,]
        
        #Obtenemos la que sería la siguiente fase
        siguiente_fase_pedido <- fase.getSiguienteFase(fase_a_revisar$Fase)
        
        #Tomo solo los pedidos de la fase a revisar
        pedidos_de_la_fase <- pedidos[pedidos$fase==fase_a_revisar$Fase,]
        
        #Vemos si existen los pedidos
        if(exists("pedidos_de_la_fase")){
          if(nrow(pedidos_de_la_fase)>0){
            
            #Cogemos uno por uno los pedidos que se encuentran en la fase indicada
            for(i in 1:nrow(pedidos_de_la_fase)){
              #Tiempo que lleva procesandose el pedido
              tiempo_proceso_pedido <- pedidos_de_la_fase[i,]$tiempo_procesamiento_fase
              #Tiempo que necesitamos para procesar un pedido en esa fase
              tiempo_proceso_fase <- fase_a_revisar$Tiempo_proceso
              #Obtenemos el id de Subpedido
              indicePedido <- pedidos_de_la_fase[i,]$idSubpedido
              
              #20170428 Modificación para pasar a fase final solo si ha llegado a la hora de la recogida
              #if(tiempo_proceso_fase <= tiempo_proceso_pedido){
              if(tiempo_proceso_fase <= tiempo_proceso_pedido)
              {
                
                #Obtenemos la que sería la siguiente fase
                #La sacamos fuera para calcularla solo una vez, ya que vamos haciendo el barrido por fases
                #siguiente_fase_pedido <- fase.getSiguienteFase(fase_a_revisar$Fase)
                
                #Comprobamos si la siguiente fase es un almacen
                #Si es un almacen vemos la posibilidad de salir del almacen
                if(siguiente_fase_pedido$Tipo == "Almacen"){
                  enAlmacen <- TRUE
                  #Si es un almacen creamos una siguiente fase postAlmacen
                  siguiente_fase_pedido_postAlmacen <- fase.getSiguienteFase(siguiente_fase_pedido$Fase)
                  #Vemos si hay recursos disponibles en la siguiente fase y si los hay saltamos la fase de almacen
                  hayRecursosDisponibles <- recursos.checkDisponibles(fase = siguiente_fase_pedido_postAlmacen$Fase, pedido = indicePedido)
                  
                  #Comprobamos si hay recursos disponibles después del almacen
                  if (hayRecursosDisponibles){
                    enAlmacen <- FALSE
                    siguiente_fase_pedido <- siguiente_fase_pedido_postAlmacen
                  } else { #Si no hay recursos disponibles para después del almacen tratamos de dejarlo en el almacen
                    hayRecursosDisponibles <- recursos.checkDisponibles(fase = siguiente_fase_pedido$Fase, pedido = indicePedido)
                  }

                }else{
                  enAlmacen <- FALSE
                  hayRecursosDisponibles <- recursos.checkDisponibles(fase = siguiente_fase_pedido$Fase, pedido = indicePedido)
                }
                
                print(paste(indicePedido," - Hay recursos disponibles:",hayRecursosDisponibles))
                
                #Modificamos para que no pase a fase Cobrar si no se puede
                #if(hayRecursosDisponibles | siguiente_fase_pedido == "Final")
                if((hayRecursosDisponibles & siguiente_fase_pedido != "Cobrar") | siguiente_fase_pedido == "Final" | 
                  (siguiente_fase_pedido == "Cobrar" & pedidos_de_la_fase[i,]$hora_recogida <= minutos_to_horas(hora_inicio+paso))){
                  
                  #Liberar los recursos usados
                  print(paste("Indice pedido:", indicePedido, sep=" "))
                  #recursos <<- recursos.liberar(pedidos[indicePedido,])
                  recursos <<- recursos.liberar(pedidos[pedidos$idPedido==indicePedido,])
                  
                  #fase_anterior <- pedidos[indicePedido,]$fase
                  fase_anterior <- pedidos[pedidos$idPedido==indicePedido,]$fase
                  #Pasar de fase el pedido
                  pedidos.setFaseParaPedido(indicePedido,siguiente_fase_pedido$Fase)

                  #Ocupar los recursos, si no es la última fasecontrolle
                  if(siguiente_fase_pedido != "Final")
                    #recursos <<- recursos.ocupar(pedidos[indicePedido,])
                    recursos <<- recursos.ocupar(pedidos[pedidos$idPedido==indicePedido,])
                  
                } else {
                  #Liberamos a las Personas siempre cuando se ha terminado un proceso, 
                  #el producto en proceso se puede quedar en las máquinas
                  print(paste("Indice pedido:", indicePedido, sep=" "))
                  #recursos <<- recursos.liberarPersonas(pedidos[indicePedido,])
                  recursos <<- recursos.liberarPersonas(pedidos[pedidos$idPedido==indicePedido,])
                }
              }

            }    
          }  
        }
        
        
      } #for n
      
      #Incrementa en un paso el pedido y los recursos asociados al pedido
      pedidos <<- pedidos.incrementarPaso()

      #Guardar el historico de ese paso
      pedidos.guardarHistorico(marcaTiempo = minutos_to_horas(hora_inicio+paso), simulacion = idSimulacion)
      
      #Retirar pedidos que han finalizado
      #Da problemas, ya que el siguiente pedido que entra coge el primer valor libre
      pedidos <<- pedidos.retirarFinalizados()
    }

  }

  #incrementar los ciclos de los recursos
  recursos <<- recursos.incrementarCiclos()
  
  #Guardar el historico de ese paso
  recursos.guardarHistorico(marcaTiempo = minutos_to_horas(paso), simulacion = idSimulacion)

  if(debug){
    print("Debug Controller:")
    if(exists("pedidos")) print(pedidos)
    print(recursos)
    # print(colaRecursos)
    readline()
  }
  
  
}



controller.simulate <- function(simulaciones = 1, debug = TRUE, h_inicio="20:00", h_fin="21:00"){
  
  controller.init(h_inicio, h_fin)
  
  if(num_pasos==0 || simulaciones < 1){
    return(-1)
  }
  
  for(simulacion in 1:simulaciones){
    
    #Iniciamos los recursos para la simulacion
    recursos.init()
    
    for(i in 1:num_pasos){
      print(paste("Progreso:",i/num_pasos,"%"))
      print(minutos_to_horas(hora_inicio+i))
      controller.ejecuta_paso(i, simulacion, debug = debug)
    }  
  
    #Eliminamos los pedidos
    pedidos.limpiar()
    #Eliminamos los recursos
    recursos.limpiar()
  }
  
    return(1)  
}