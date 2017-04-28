shinyUI(pageWithSidebar(
  headerPanel("Kristen Cookies Launch App"),
  sidebarPanel(
    h1('Input Panel'),
    radioButtons("debug", "Debug mode:", c("Normal"=FALSE, "Debug"=TRUE)),
    textInput("start_hour", "Start hour", "20:00"),
    textInput("finish_hour", "Finish hour", "21:00"),
    sliderInput("minutes_per_step", "Number of minutes per step:", 1, min=1, max=5, step=1),
    sliderInput("simulations", "Number of simulations", 1, min=1, max=100, step=5),
    actionButton("execute", "Launch Simulation"),
    h2("Instructions:"),
    tabsetPanel(
      tabPanel("Execution",
               p("1. "),
               p("2. Input your height into 'Your height' section")
      ),
      tabPanel("Plots",
               p("1. "),
               p("2. Set Lower, Upper and Obesity limits.")
      )
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Simulation Execution",
               h4("Recursos"),
               verbatimTextOutput("recursos"),
               h4("Pedidos"),
               verbatimTextOutput("pedidos")
               
      ),
      tabPanel("Config",
               h4("Debug mode:"),
               verbatimTextOutput("debug"),
               h4("Start Hour:"),
               verbatimTextOutput("start_hour"),
               h4("Finish Hour:"),
               verbatimTextOutput("finish_hour"),
               h4("Minutes per Step:"),
               verbatimTextOutput("minutes_per_step"),
               h4("Number of Simulations:"),
               verbatimTextOutput("simulations")
      )
    )
  )
)
)