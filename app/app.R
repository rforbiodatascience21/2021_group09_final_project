

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),
    navbarPage(
        "Group 9 Final Project",
        tabPanel("Breast cancer data",
                 sidebarPanel(
                     tags$h3("Modify your dataset"),
                     selectInput("dataset","Select your dataset",dataset.list),
                     radioButtons("display_mode","Display",preview_mode.list),
                     selectInput("columns","Select the columns you want to view or modify", columns.list, multiple = TRUE),
                     h4("Filter"),
                     sliderInput("age", "Age", value = c(10, 20), min = 0, max = 100),
                     textInput("patient_id","Write the patients id"),
                     radioButtons("gender", "Gender", c("male", "female"))
                     
                 ),
                 mainPanel(
                     verbatimTextOutput("table")
                 ))#end of the tabpanel
        
    ) # navbarPage end
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
