#VZM
# 12/02/2018

# coursera Data products final project

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Compound Poisson Distribution"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_breaks","number of bins", 
                  step=1,min=5, max=100, value = 20),
      numericInput("lambda", label = "Lambda: mean of Poisson Dist.",
                   min = 1, max = 100, value = 3),
      numericInput("alpha", label = "Alpha: shape factor for Gamma Dist.",
                   min = 1, max = 20000, value = 20),
      numericInput("beta", label = "Beta: rate factor for Gamma Dist.",
                   min = 1, max = 1000, value = 10)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot2"),
      textOutput("summ")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  events_sev <- reactive(rgamma(10000,input$alpha,rate=input$beta))
  set.seed(1)
  events_ct <- reactive(rpois(10000,input$lambda))
  output$distPlot <- renderPlot({
    
    
    # events_sev <<- rgamma(10000,input$alpha,rate=input$beta)
    par(mfrow=c(1,2)) 
    hist(events_ct(), probability = TRUE, breaks = as.numeric(input$n_breaks), 
         xlab = "Number of occurences", main = "Number of occurences histogram (Poisson)")
    hist(events_sev(), probability = TRUE, breaks =
           as.numeric(input$n_breaks), 
         xlab = "Severity", main = "Severity histogram (Gamma)")
  })
  output$distPlot2 <- renderPlot({
    
    hist(events_ct()*events_sev(), probability = TRUE, breaks = as.numeric(input$n_breaks), 
         xlab = "Total annual rainfall", main = "Histogram of total annual rainfal")
  })
  output$summ <- renderText({
    paste("Mean: ", mean(events_ct()*events_sev()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)