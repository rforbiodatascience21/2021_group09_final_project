

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
dataset<-("breast_cancer_dataset")
preview_mode<-c("Preview", "summary")
columns <- c("patient_id", "education_type","birth_date", "age", "weight", "thickness_tumor")


ui <- fluidPage(
    theme = shinytheme("flatly"),
    navbarPage(
        "Group 9 Final Project",
        tabPanel("Breast cancer data",
                 sidebarPanel(
                     tags$h3("Modify your dataset"),
                     selectInput("dataset","Select your dataset",dataset),
                     radioButtons("display_mode","Display",preview_mode),
                     selectInput("columns","Select the columns you want to view or modify",columns, multiple = TRUE),
                     h4("Filter"),
                     sliderInput("age", "Age", value = c(10, 20), min = 0, max = 100),
                     textInput("patient_id","Write the patients id"),
                     radioButtons("gender", "Gender", c("male", "female")),
                     actionButton("reset", "Reset")
                 ),
                 mainPanel(
                     dataTableOutput("Table"),
                     
                 )),#end of the tabpanel
        tabPanel("Data information",
                 verbatimTextOutput("summary"))#end of the tabpanel
        
    ) # navbarPage end
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
