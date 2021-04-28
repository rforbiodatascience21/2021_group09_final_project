# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)
require(nnet)


# Define functions --------------------------------------------------------




# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")



# Split into training and test --------------------------------------------

col_of_interest = c(
  "patient_id",  
  "education",
  "age",
  "weight",
  "thickness_tumor",
  "Benign_malignant_cancer",
  "hereditary_history",
  "blood",
  "taking_heartMedicine",
  "taking_blood_pressure_medicine",
  "taking_gallbladder_disease_medicine",
  "smoking",
  "alcohol",
  "radiation_history",
  "Birth_control",
  "menstrual_age",
  "menopausal_age",
  "pregnency_experience",
  "abortion",
  "breast_pain"
)

analysis_df <- my_data_clean_aug %>%
  drop_na() %>%
  droplevels.data.frame() %>%
  select(all_of(col_of_interest))

# COnsider doing a proper k-fold series to estimate generalisation error



# Training Maximum and Reduced model --------------------------------------


train <- sample_frac(analysis_df, 0.7)

test <- analysis_df %>%
  anti_join(train, b="patient_id")


multinom.fit <- multinom(Benign_malignant_cancer ~ . -patient_id -1, data=train) #All variables except patient ID and bias

multinom.fit.reduced <- multinom.fit
multinom.fit.reduced <- step(multinom.fit.reduced, trace=FALSE)


summary(multinom.fit)
summary(multinom.fit.reduced)



# Testing models ----------------------------------------------------------

# Accuracy on training set:

train <- train %>%
  mutate(
    "Max_pred" = as.factor(predict(multinom.fit, newdata = ., "class")),
    "Red_pred" =as.factor(predict(multinom.fit.reduced, newdata = ., "class"))
    ) %>%
  drop_na()

caret::confusionMatrix(pluck(train, "Max_pred"), pluck(train,"Benign_malignant_cancer"))
caret::confusionMatrix(pluck(train, "Red_pred"), pluck(train,"Benign_malignant_cancer"))

# Accuracy on test set:
test <- test %>%
  mutate(
    "Max_pred" = predict(multinom.fit, newdata = ., "class"),
    "Red_pred" = predict(multinom.fit.reduced, newdata = ., "class")
  ) %>%
  drop_na()


caret::confusionMatrix(pluck(test, "Max_pred"), pluck(test,"Benign_malignant_cancer"))
caret::confusionMatrix(pluck(test, "Red_pred"), pluck(test,"Benign_malignant_cancer"))

