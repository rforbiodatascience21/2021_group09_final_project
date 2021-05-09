# Load R packages
library(shiny)
library(shinythemes)
library(devtools)
library(tidyverse)

# Run Model script
setwd("/cloud/project")
source(file = "R/11_multinomial_log_reg_model_condition.R")


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
                     selectInput("education", "Education level:", c("Illiterate","Elementary","Middle School","High School","Diploma","Associate","Bachelor","Master"),"High School"),
                     textInput("age", "Age:", "36"),
                     textInput("weight", "Weight (kg):", "60"),
                     textInput("tumor_size", "Tumor Thickness (cm):", "0.9"),
                     radioButtons("tumor_type", "Tumor Type:", c("Benign","Malignant"),inline=TRUE),
                     selectInput("blood", "Blood Type:", c("A+","A-","B+","B-","AB+","AB-","O+","O-")),
                     radioButtons("heartMedicine", "Heart Medicine:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("hereditary", "Hereditary History:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("blood_pressure_medicine", "Blood Pressure Medicine:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("gallbladder_medicine", "Gallbladder Medicine:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("smoking", "Smoking:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("alcohol", "Alcohol:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("radiation", "Radiation:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("birth_control", "Birth Control:", c("No"=0,"Yes"=1),inline=TRUE),
                     selectInput("menstrual", "Menstrial Age:", c("under 12","above 12","not yet")),
                     selectInput("menopause", "Menopause Age:", c("under 50","above 50","not yet")),
                     radioButtons("pregnency", "Pregnancy Experience:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("abortion", "Abortion Experience:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("breast_pain", "Breast Pain:", c("No"=0,"Yes"=1),inline=TRUE)
                    
                 ), # sidebarPanel
                 mainPanel(HTML(paste(h1("Description"),
                           ("This function calculates an individual’s chance of surviving breast cancer. The predictions are based on a machine learning algorithm, that is fitted to a dataset consisting of 1134 previous breast cancer patients and 30 different personal factors."),'<br/>',
                           ("To calculate your chance, insert your personal information in the fields in the side panel ‘User Input’ (RT). For more directions look in the tab “Field Information”."),
                     h4("Survival Rate (%): "))),
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
    observe({unknown_input <- tibble_row("patient_id"="111035903832",  
                        "education"=input$education,
                        "age"=as.numeric(input$age),
                        "weight"=as.numeric(input$weight),
                        "thickness_tumor"=as.numeric(input$tumor_size),
                        "Benign_malignant_cancer"=input$tumor_type,
                        "hereditary_history"=input$hereditary,
                        "blood"=input$blood,
                        "taking_heartMedicine"=input$heartMedicine,
                        "taking_blood_pressure_medicine"=input$blood_pressure_medicine,
                        "taking_gallbladder_disease_medicine"=input$gallbladder_medicine,
                        "smoking"=input$smoking,
                        "alcohol"=input$alcohol,
                        "radiation_history"=input$radiation,
                        "Birth_control"=input$birth_control,
                        "menstrual_age"=input$menstrual,
                        "menopausal_age"=input$menopause,
                        "pregnency_experience"=input$pregnency,
                        "abortion"=input$abortion,
                        "breast_pain"=input$breast_pain,
                        "condition"="0")
    output$model_prediction<- renderText(round(predict(multinom.fit, newdata = unknown_input, type="probs"),4)*100)
        })
    #output$model_prediction <- renderText("HEJ")
    output$field_tbl <- renderTable({field_tibble},  
                                    striped = TRUE,  
                                    spacing = 'l',
                                    align = 'l') 
} # Server end

# Run the application 
shinyApp(ui = ui, server = server)
