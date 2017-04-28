shinyServer(
  function(input, output) {
    source("controller.R", local=TRUE)
    
    output$debug <- renderPrint({input$debug})
    output$start_hour <- renderPrint({input$start_hour})
    output$finish_hour <- renderPrint({input$finish_hour})
    output$minutes_per_step <- renderPrint({input$minutes_per_step})
    output$simulations <- renderPrint({input$simulations})
    
    output$height <- renderPrint({round(input$height,2)})
    output$weight <- renderPrint({round(input$weight,1)})

    observeEvent(input$execute, {
      controller.simulate(input$simulations, input$debug)
      output$recursos <- renderPrint({print(recursos)})
      output$pedidos <- renderPrint({print(pedidos)})
    })

  })