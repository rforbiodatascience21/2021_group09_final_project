# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)
library(broom)
require(nnet)
set.seed(777)



# Load data ---------------------------------------------------------------
data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")



# Split into training and test --------------------------------------------

col_of_interest = c(
  "patient_id",  
  "education",
  "age",
  "weight",
  "thickness_tumor",
  #"Benign_malignant_cancer",
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
  "breast_pain",
  "condition"
)

analysis_df <- data_clean_aug %>%
  filter(condition != "under treatment") %>%
  select(all_of(col_of_interest)) %>%
  drop_na() %>%
  droplevels.data.frame()

# COnsider doing a proper k-fold series to estimate generalisation error



# Training Maximum and Reduced model --------------------------------------


train <- sample_frac(analysis_df, 0.7)

test <- analysis_df %>%
  anti_join(train, b="patient_id")

baseline = DescTools::Mode(pluck(test, "condition"))

multinom.fit <- multinom(condition ~ . -patient_id -1, data=train) #All variables except patient ID and bias

multinom.fit.reduced <- multinom.fit
multinom.fit.reduced <- step(multinom.fit.reduced, trace=FALSE)


summary(multinom.fit)
summary(multinom.fit.reduced)



# Testing models ----------------------------------------------------------


# Prediction
test <- test %>%
  mutate(
    "Max_pred" = predict(multinom.fit, newdata = ., "class"),
    "Red_pred" = predict(multinom.fit.reduced, newdata = ., "class"),
    "baseline" = baseline
  ) %>%
  drop_na()

# Evaluation of testing
suma <- test %>%
  summarize(
    "Max_pred" = tidy(caret::confusionMatrix(Max_pred, condition)),
    "Red_pred" = tidy(caret::confusionMatrix(Red_pred, condition)),
    "baseline" = tidy(caret::confusionMatrix(baseline, condition))
  ) %>%
  pivot_longer(c(Max_pred, Red_pred, baseline), names_to = "model", values_to = "terms") %>%
  bind_cols(pluck(., "terms")) %>%   # couldn't be done with unnest() since "terms" had dim= [,6] [28,6]?
  select(-terms) %>%
  select(model, term, class, estimate) %>%
  filter(term %in% c("balanced_accuracy", "sensitivity", "specificity"))



# Writing performance and model -------------------------------------------

write.csv(suma, "results/08_Model_performance_condition.csv", row.names = FALSE)
saveRDS(multinom.fit, "results/08_maxModel_condition.RDS")
saveRDS(multinom.fit.reduced, "results/08_redModel_condition.RDS")
