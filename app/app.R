# Load R packages
library(shiny)
library(shinythemes)
library(devtools)
library(tidyverse)



# Define UI for application that draws a histogram
field_tibble <- tibble(
    "Field Name" = c("Education level","Age","Weight"), 
    "Description" = c("The education level of the patient.","The age of the patient in years.","The weight of the patient in kilograms."), 
    "Range/Values" = c(" Illiterate=0, Elementary= 1, Middle School =2 , High School =3 , Diploma = 4, Associate =5 , Bachelor =6 , Master = 7","20 - 45","0 - 1000"))

ui <- fluidPage(
    theme = shinytheme("flatly"),
    navbarPage(
        "Breast Cancer Survival Rate",
        tabPanel("Calculator",
                 sidebarPanel(
                     tags$h3("User Input:"),
                     textInput("input_education", "Education level:", "0"),
                     textInput("input_age", "Age:", "35"),
                     textInput("input_weight", "Weight (kg):", "60"),
                     textInput("input_tumor_size", "Tumor Thickness (cm):", "0.9"),
                     radioButtons("input_tumor_type", "Tumor Type:", c("benign","malignant"),inline=TRUE),
                     selectInput("input_blood", "Blood Type:", c("A+","A-","B+","B-")),
                     radioButtons("input_heartMedicine", "Heart Medicine:", c("No","Yes"),inline=TRUE),
                     radioButtons("input_blood_pressure_medicine", "Blood Pressure Medicine:", c("No","Yes"),inline=TRUE),
                     radioButtons("input_gallbladder_medicine", "Gallbladder Medicine:", c("No","Yes"),inline=TRUE),
                     radioButtons("input_smoking", "Smoking:", c("No","Yes"),inline=TRUE),
                     radioButtons("input_alcohol", "Alcohol:", c("No","Yes"),inline=TRUE),
                     radioButtons("input_radiation", "Radiation:", c("No","Yes"),inline=TRUE),
                     radioButtons("input_birth_control", "Birth Control:", c("No","Yes"),inline=TRUE),
                     textInput("input_menstrual", "Menstrial Age:", "12"),
                     textInput("input_menopause", "Menopause Age:", "50"),
                     radioButtons("input_pregnency", "Pregnancy Experience:", c("No","Yes"),inline=TRUE),
                     radioButtons("input_abortion", "Abortion Experience:", c("No","Yes"),inline=TRUE),
                     radioButtons("input_breast_pain_rb", "Breast Pain:", c("No","Yes"),inline=TRUE)
                    
                 ), # sidebarPanel
                 mainPanel(HTML(paste(h1("Description"),
                           ("This function calculates an individual’s chance of surviving breast cancer and displays the main risk factors. The predictions are based on a machine learning algorithm, that is fitted to a dataset consisting of 1134 previous breast cancer patients and 30 different personal factors."),'<br/>',
                           ("To calculate your chance, insert your personal information in the fields in the side panel ‘User Input’ (RT). For more directions look in the tab “Field Information”."),
                     h4("Survival Rate: "))),
                     verbatimTextOutput("model_prediction")
                 )),
        tabPanel("Field information",
                 h3("Desription"),
                 "Descriptions of each of the personal factors (fields for the calculator) are listed in the table below.",
                 tableOutput('field_tbl')
                 )
    ) # navbarPage end
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$field_tbl <- renderTable({field_tibble},  
                                    striped = TRUE,  
                                    spacing = 'l',
                                    align = 'l') 
} # Server end

# Run the application 
shinyApp(ui = ui, server = server)
