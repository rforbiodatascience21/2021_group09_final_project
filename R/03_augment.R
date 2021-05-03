# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
clean_data <- read_csv(file = "data/02_clean_combined_cases.csv")



# Wrangle data ---------------------------------------------------------------


aug_data <- clean_data %>%
  # Add names to the 'wierd' categories 
  mutate("education" = case_when(education == 0 ~ "Illiterate",
                                    education == 1 ~ "Elementary",
                                    education == 2 ~ "Middle School",
                                    education == 3 ~ "High School",
                                    education == 4 ~ "Diploma",
                                    education == 5 ~ "Associate",
                                    education == 6 ~ "Bachelor",
                                    education == 7 ~ "Master")) %>%
           
  mutate("blood" = case_when(blood == 0 ~ "A+",
                                  blood == 1 ~ "A-",
                                  blood == 2 ~ "AB+",
                                  blood == 3 ~ "AB-",
                                  blood == 4 ~ "B+",
                                  blood == 5 ~ "B-",
                                  blood == 6 ~ "O+",
                                  blood == 7 ~ "O-")) %>%
           
  mutate("age_FirstGivingBirth" = case_when(age_FirstGivingBirth == 0 ~ "under 30",
                                            age_FirstGivingBirth == 1 ~ "above 30")) %>%
  
  mutate("menstrual_age" = case_when(menstrual_age == 0 ~ "not yet",
                                     menstrual_age == 1 ~ "under 12",
                                     menstrual_age == 2 ~ "above 12")) %>%
  
  mutate("menopausal_age" = case_when(menopausal_age == 0 ~ "not yet",
                                       menopausal_age == 1 ~ "under 50",
                                       menopausal_age == 2 ~ "above 50")) %>%
  
  mutate("Benign_malignant_cancer" = case_when(Benign_malignant_cancer == 0 ~ "Benign",
                                               Benign_malignant_cancer == 1 ~ "Malignant")) %>%
  
  mutate("marital_length" = case_when(marital_length == 0 ~ "under 10 years",
                                      marital_length == 1 ~ "above 10 years")) %>%

  # Add new columns
  mutate(
    treatment_age = treatment_data-as.numeric(birth_date)) %>%
  
  # Change to categories
  mutate(across(
    .cols = -c(age, treatment_age, weight, thickness_tumor, birth_date),
    as.factor)) %>%
  
  # Filter out men
  filter(gender == 0) %>%
  
  # Remove singular columns
  select_if(function(col) length(unique(col)) > 1) 

# Remove inconsistencies in the data
  # Based on UK statistics, only look at women above 20 years old
  # (https://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/breast-cancer/incidence-invasive#heading-One)
  # we still need a source for removing low weight people: now below 35kg is 'randomly' chosen

aug_data <- aug_data %>%
  filter(age >= 20, weight >= 35) 



# Write data ---------------------------------------------------------------

saveRDS(aug_data, file = "data/03_clean_augmented_combined_breastcancer_data.rds")

