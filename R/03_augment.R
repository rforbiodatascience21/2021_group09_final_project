# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
dead_cases <- read_csv(file = "data/02_clean_dead_cases.csv")
recovered_cases <- read_csv(file = "data/02_clean_recovered_cases.csv")
under_treatment_cases <- read_csv(file = "data/02_clean_under_treatment_cases.csv")


# Wrangle data ---------------------------------------------------------------

# Join data sets on all columns (as all columns are identical)
dead_cases<- dead_cases %>%
  mutate(birth_date=as.double(birth_date))

clean_data <- dead_cases %>% 
  full_join(recovered_cases) %>%
  full_join(under_treatment_cases) %>%
  rename(Birth_control = 'Birth_control(Contraception)') %>%
  # Add names to categories
  mutate("education_type" = case_when(education == 0 ~ "Illiterate",
                                    education == 1 ~ "Elementary",
                                    education == 2 ~ "Middle School",
                                    education == 3 ~ "High School",
                                    education == 4 ~ "Diploma",
                                    education == 5 ~ "Associate",
                                    education == 6 ~ "Bachelor",
                                    education == 7 ~ "Master"),
         .after = education) %>%
  mutate("blood_type" = case_when(blood == 0 ~ "A+",
                                  blood == 1 ~ "A-",
                                  blood == 2 ~ "AB+",
                                  blood == 3 ~ "AB-",
                                  blood == 4 ~ "B+",
                                  blood == 5 ~ "B-",
                                  blood == 6 ~ "O+",
                                  blood == 7 ~ "O-"),
         .after = blood) %>%
  filter(gender == 0) %>%
  # Change to categories
  mutate(hereditary_history = as.factor(hereditary_history),
         marital_status = as.factor(marital_status),
         marital_length = as.factor(marital_length),
         pregnency_experience = as.factor(pregnency_experience),
         giving_birth = as.factor(giving_birth),
         age_FirstGivingBirth = as.factor(age_FirstGivingBirth),
         abortion = as.factor(abortion),
         taking_blood_pressure_medicine = as.factor(taking_blood_pressure_medicine),
         taking_gallbladder_disease_medicine = as.factor(taking_gallbladder_disease_medicine),
         smoking = as.factor(smoking),
         taking_heartMedicine = as.factor(taking_heartMedicine),
         alcohol = as.factor(alcohol),
         breast_pain = as.factor(breast_pain),
         radiation_history = as.factor(radiation_history),
         Birth_control = as.factor(Birth_control),
         menstrual_age = as.factor(menstrual_age),
         menopausal_age = as.factor(menopausal_age),
         Benign_malignant_cancer = as.factor(Benign_malignant_cancer)
         )
  

# Fix warning; clean_data


# Write data ---------------------------------------------------------------

save(clean_data, file = "data/03_clean_augmented_combined_breastcancer_data.RData")

