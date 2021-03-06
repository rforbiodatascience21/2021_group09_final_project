# Clear Workspace ---------------------------------------------------------
rm(list = ls())


# Load Libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
dead_cases <- read_csv("data/01_death_cases.csv", 
                       na = "-")

recovered_cases <- read_csv("data/01_recovered_cases.csv", 
                            na = "-")

under_treatment_cases <- read_csv("data/01_under_treatment_cases.csv", 
                                  na = "-")


# Join the files ------------------------------------------------------------
dead_cases <- dead_cases %>%
  mutate(across(-condition, 
                as.double))

recovered_cases <- recovered_cases %>%
  mutate(across(-condition, 
                as.double))

under_treatment_cases <- under_treatment_cases %>%
  mutate(across(-condition, 
                as.double))

comb_data <- dead_cases %>% 
  full_join(recovered_cases) %>%
  full_join(under_treatment_cases) 


# Wrangle Date ---------------------------------------------------------------
comb_clean_data <- comb_data %>%
  # Clean column names
  rename_with(.cols = everything(), 
              ~str_remove(string = .x, 
                          pattern = "[\r]?\n")) %>%
  rename(Birth_control = "Birth_control(Contraception)") %>%
  
  # Cleanup of binary variables
  mutate(across(
    .cols = c(hereditary_history, 
              marital_status, 
              marital_length,
              pregnency_experience, 
              age_FirstGivingBirth, 
              abortion, 
              taking_heartMedicine,
              taking_blood_pressure_medicine, 
              taking_gallbladder_disease_medicine, 
              smoking,
              alcohol, 
              breast_pain, 
              radiation_history, 
              Birth_control, 
              Benign_malignant_cancer),
    ~ case_when(is.na(.) ~ NA_real_,
                . == 0   ~  0,
                TRUE     ~  1)
  )) %>%
  # Cleanup of individual columns
    mutate(
      blood = ifelse(blood %in% seq(0,7), 
                     yes = blood, no = NA_real_),
      birth_date = ifelse(nchar(birth_date) == 4,
                          yes = birth_date, 
                          no = NA_real_)
    ) %>%
  # Low age and low weight
  filter(age >= 20, weight >= 35) %>%
  # Not got period yet, but gave birth (= does not make sense) (1 person)
  filter(!(menstrual_age == "not yet" & pregnency_experience == 0)) %>%
  # Filter out men
  filter(gender == 0) 

# Write data --------------------------------------------------------------

write_csv(x = comb_clean_data,
          file = "data/02_clean_combined_cases.csv")


