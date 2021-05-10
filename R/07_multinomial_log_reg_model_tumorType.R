# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
require("nnet")
set.seed(777)

# Load data ---------------------------------------------------------------
data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")

# Extract features of interest --------------------------------------------

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
  #"radiation_history",
  "Birth_control",
  "menstrual_age",
  "menopausal_age",
  "pregnency_experience",
  "abortion",
  "breast_pain"
)

analysis_df <- data_clean_aug %>%
  drop_na() %>%
  droplevels.data.frame() %>%
  select(all_of(col_of_interest)) %>%
  mutate(
    Benign_malignant_cancer = relevel(Benign_malignant_cancer, 
                                      "Malignant")
  )

# Split data into test and train sets --------------------------------------

train <- sample_frac(analysis_df, 0.7)

test <- analysis_df %>%
  anti_join(train, 
            b = "patient_id")

# Fit models to train set Maximum and Reduced model --------------------------------------

multinom.fit <- multinom(Benign_malignant_cancer ~ . -patient_id -1, 
                         data = train) #All variables except patient ID and bias

multinom.fit.reduced <- multinom.fit
multinom.fit.reduced <- step(multinom.fit.reduced, 
                             trace=FALSE)


summary(multinom.fit)
summary(multinom.fit.reduced)

# Testing models ----------------------------------------------------------

# Prediction
test <- test %>%
  mutate(
    "Max_pred" = predict(multinom.fit, 
                         newdata = ., "class"),
    "Red_pred" = predict(multinom.fit.reduced, 
                         newdata = ., "class"),
    "baseline" = DescTools::Mode(pluck(test, 
                                       "Benign_malignant_cancer"))
  ) %>%
  drop_na()

# Evaluation of models ----------------------------------------------------------

suma <- test %>%
  summarize(
    "Max_pred" = tidy(caret::confusionMatrix(Max_pred, 
                                             Benign_malignant_cancer)),
    "Red_pred" = tidy(caret::confusionMatrix(Red_pred, 
                                             Benign_malignant_cancer)),
    "baseline" = tidy(caret::confusionMatrix(baseline, 
                                             Benign_malignant_cancer))
  ) %>%
  pivot_longer(c(Max_pred, 
                 Red_pred, 
                 baseline), 
               names_to = "model", 
               values_to = "terms") %>%
  bind_cols(pluck(., 
                  "terms")) %>%   # couldn't be done with unnest() since "terms" had dim= [,6] [28,6]?
  select(-terms) %>%
  select(model, 
         term, 
         class, 
         estimate) %>%
  filter(term %in% c("balanced_accuracy", 
                     "sensitivity", 
                     "specificity"))

# Writing performance and  model ------------------------------------------

write.csv(suma, 
          "results/07_Model_performance_tumor.csv",
          row.names = FALSE)
saveRDS(multinom.fit, 
        "results/07_maxModel_tumor.RDS")
saveRDS(multinom.fit.reduced, 
        "results/07_redModel_tumor.RDS")

