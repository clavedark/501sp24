################
#shiny app to simulate central limit theorem
#dc - 2.11.24
################
#install.packages("mvtnorm")
library(shiny)
library(ggplot2)
library(mvtnorm)
library(patchwork)

# Define UI for central limit theorem
ui <- fluidPage(

    # Application title
    titlePanel("Simulating the Central Limit Theorem"),

    # slider for number of variables; radio buttons for distribution type 
    sidebarLayout(
        sidebarPanel(
            sliderInput("N",
                        "Number of Variables, N:",
                        min = 0,
                        max = 2500,
                        value = 15, step=10),
            
            br(),
            
            radioButtons("dist", "Distribution type:",
                         choices = c("Normal" = "norm", "Binomial" = "binom",
                                        "Poisson" = "poisson"))),
        
        mainPanel(
          "Drawing N samples from any distribution, the means of those N variables will be normally distributed as N increases.",
          br(),
           plotOutput("distPlot")
           
          
          
    ))
)

# Server to simulate central limit theorem by chosen distribution
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # generate parameters based on input$  from ui.R
      dist <- switch(input$dist,
                     norm = rnorm,
                     binom = rbinom,
                     poisson = rpois,
                     rnorm)
      d <-0
for (i in 1:input$N) {
  d[i] <- ifelse(input$dist=="binom", mean(dist(1000, 100, .5)), ifelse(input$dist=="poisson", mean(dist(1000, 1)), mean(dist(1000))))  
}
  x <-data.frame(d)    

  ggplot(data=x, aes(x=d)) + geom_histogram()+
    labs(title="Histogram of means from N variables", x="x", y="Frequency")
      
 
}
)
}

# Run the application 
shinyApp(ui = ui, server = server)
