# Load R packages
library(shiny)
library(shinythemes)
library(devtools)
library(tidyverse)
require(nnet)
library(rsconnect)

# Load model 
multinom.fit.reduced <- readRDS(file = "08_redModel_condition.rds")

# Define UI
ui <- fluidPage(
    theme = shinytheme("flatly"),
    navbarPage( "Breast Cancer Survival Rate",
        tabPanel("Calculator",
                 sidebarPanel(tags$h3("User Input:"),
                     textInput("age", "Age:", "36"),
                     textInput("weight", "Weight (kg):", "60"),
                     selectInput("menstrual", "Menstrual Age:", c("under 12","above 12","not yet")),
                     selectInput("given_birth", "Given Birth:", c("0","1","2","3","4","5","6","7")),
                     selectInput("education", "Education level:", c("Illiterate","Elementary","Middle School","High School","Diploma","Associate","Bachelor","Master"),"High School"),
                     radioButtons("tumor_type", "Tumor Type:", c("Benign","Malignant"),inline=TRUE),
                     radioButtons("hereditary", "Hereditary History:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("alcohol", "Alcohol:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("radiation", "Radiation:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("abortion", "Abortion Experience:", c("No"=0,"Yes"=1),inline=TRUE),
                     radioButtons("breast_pain", "Breast Pain:", c("No"=0,"Yes"=1),inline=TRUE)
                 ), # sidebarPanel
                 
                 mainPanel(h3("Description"),
                           h5("This function calculates an individual’s chance of surviving breast cancer. The predictions are based on a machine learning algorithm, that is fitted to a dataset consisting of 1134 previous breast cancer patients and 30 different personal factors."),
                           h5("To calculate your chance, insert your personal information in the fields in the side panel ‘User Input’ (RT). For more directions look in the tab “Field Information”."),
                           h3("Survival Rate (%):"),
                           verbatimTextOutput("model_prediction"),
                           h6("Disclaimer: Interpret the predictions with a pinch of salt. The model has an accuracy of 0.69.")
                 )), # mainPanel, tabPanel(calculator)
        
        tabPanel("Field information",
                 h3("Desription"),
                 h5("Descriptions of each of the personal factors (fields for the calculator) are listed in the table below."),
                 tableOutput('field_tbl'),
                 h6("* For the fields 'Age' and 'weight' suggestive ranges are listed. These ranges match with the analysis conducted. Value outside of these ranges yields inaccurate predictions.")
                 ) # tabPanel (Field Information)
    )) # navbarPage end, UI end  

# Define server logic
server <- function(input, output) {
    observe({unknown_input <- tibble_row(
                        "age"=as.numeric(input$age),
                        "education"=input$education,
                        "Benign_malignant_cancer"=input$tumor_type,
                        "weight"=as.numeric(input$weight),
                        "hereditary_history"=input$hereditary,
                        "alcohol"=input$alcohol,
                        "radiation_history"=input$radiation,
                        "menstrual_age"=input$menstrual,
                        "giving_birth"=input$given_birth,
                        "abortion"=input$abortion,
                        "breast_pain"=input$breast_pain)
    output$model_prediction<- renderText(round(predict(multinom.fit.reduced, newdata = unknown_input, type="probs"),4)*100)
        })
    field_tibble <- tibble(
        "Field Name" = c("Age", "Weight", "Menstrual Age","Given Birth","Education","Tumor Type","Hereditary History", "Alcohol",
                         "Radiation", "Abortion Experience", "Breast Pain"), 
        "Description" = c("The age of the patient in years.","The weight of the patient in kilograms.","At which age group did the patient start a natural menstrual cycle.",
                          "Number of times the patient has given birth","The education level of the patient.","If the tumor is benign (noncancerous) or malignant (cancerous)",
                          "History of breast cancer in the family", "Alchol Habits","Radiation therapy in the breast area",
                          "Abortion history","Any kind of pain in the breast tissue."),
        "Range/Values" = c("Range: 20 - 45","Range: 35 - 150", str_c("Categorical:","- Not Yet (cycle not started)", "- Under 12", "- Above 12",sep="<br>"),
                           "Range: 0-7"," Illiterate=0, Elementary= 1, Middle School =2 , High School =3 , Diploma = 4, Associate =5 , Bachelor =6 , Master = 7",str_c("Categorical:","- Bening", "- Malignant",sep="<br>"),
                           str_c("Categorical:","- Yes (Breast cancer in the family)", "- No (No Family History )",sep="<br>"),str_c("Categorical:","- Yes", "- No",sep="<br>"),
                           str_c("Categorical:","- Yes (Therapy)", "- No (No Therapy)",sep="<br>"),
                           str_c("Categorical:","- Yes (1 or more abortions)", "- No (No abortions)",sep="<br>"),str_c("Categorical:","- Yes (Breast pain of some sort)", "- No (No breast pain)",sep="<br>")))
    
    #output$model_prediction <- renderText("HEJ")
    output$field_tbl <- renderTable({field_tibble},  
                                    striped = TRUE,  
                                    spacing = 'l',
                                    align = 'l',
                                    sanitize.text.function=identity) 
} # Server end

# Run the application 
shinyApp(ui = ui, server = server)
