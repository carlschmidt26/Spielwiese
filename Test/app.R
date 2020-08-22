#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
source("../debounce_if.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("number",
                        "Number:",
                        min = 1,
                        max = 50,
                        value = 30),
            radioButtons(
                "update_necc",
                "Update??",
                choices = list("TRUE", "FALSE"),
                selected = "TRUE")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    assign("update_by_user", FALSE, envir = .GlobalEnv)
    v <- reactiveValues(trigger = NULL, when = NULL)
    
    # Re-execute when the reactive `r` becomes invalid.
    debounced <- eventReactive(input$bins, {

        print(glue::glue('Debounce at {format(Sys.time(), "%Y-%m-%d %H:%M:%OS6")}'))
       
        if (input$update_necc == "TRUE") {
            assign("update_by_user", FALSE, envir = .GlobalEnv)
            updateSliderInput(session, inputId = "number",
                              value =  sample(1:50, 1))
            print("----")
        }
        
    }) %>% debounce(2000)
    
    event <- eventReactive(input$number, {
        print(update_by_user)
        print(glue::glue('Event at {format(Sys.time(), "%Y-%m-%d %H:%M:%OS6")}'))
        #print(address(update_by_user))
        input$number
    }) %>% debounce_if(update_by_user, 2000, 0)
    
    
    observeEvent({event()},{
                 print(glue::glue('ObsereEvent at {format(Sys.time(), "%Y-%m-%d %H:%M:%OS6")}'))
    })
    
    observeEvent({debounced()},{
                 print(glue::glue('ObserveDebounce at {format(Sys.time(), "%Y-%m-%d %H:%M:%OS6")}'))
                 })
    
    # observeEvent(input$number, {
    #     print("Change detected")
    # })
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
