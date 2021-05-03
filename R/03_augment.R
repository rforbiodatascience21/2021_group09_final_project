# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
clean_data <- read_csv(file = "data/02_clean_combined_cases.csv")



# Wrangle data ---------------------------------------------------------------


aug_data <- clean_data %>%
    # Add names to categories
  mutate("education_name" = case_when(education == 0 ~ "Illiterate",
                                    education == 1 ~ "Elementary",
                                    education == 2 ~ "Middle School",
                                    education == 3 ~ "High School",
                                    education == 4 ~ "Diploma",
                                    education == 5 ~ "Associate",
                                    education == 6 ~ "Bachelor",
                                    education == 7 ~ "Master"),
         .after = education)%>%
  mutate("blood_name" = case_when(blood == 0 ~ "A+",
                                  blood == 1 ~ "A-",
                                  blood == 2 ~ "AB+",
                                  blood == 3 ~ "AB-",
                                  blood == 4 ~ "B+",
                                  blood == 5 ~ "B-",
                                  blood == 6 ~ "O+",
                                  blood == 7 ~ "O-"),
         .after = blood) %>%
  mutate("age_FirstGivingBirth_name" = case_when(age_FirstGivingBirth == 0 ~ "under 30",
                                       age_FirstGivingBirth == 1 ~ "above 30"),
         .after = age_FirstGivingBirth) %>%
  mutate("menstrual_age_name" = case_when(menstrual_age == 0 ~ "not yet",
                                     menstrual_age == 1 ~ "under 12",
                                     menstrual_age == 2 ~ "above 12"),
         .after = menstrual_age) %>%
  mutate ("menopausal_age_name" = case_when(menopausal_age == 0 ~ "not yet",
                                       menopausal_age == 1 ~ "under 50",
                                       menopausal_age == 2 ~ "above 50"),
          .after =menopausal_age) %>%
  mutate ("Benign_malignant_cancer_name" = case_when(Benign_malignant_cancer == 0 ~ "Benign",
                                    Benign_malignant_cancer == 1 ~ "Malignant"),
          .after = Benign_malignant_cancer) %>%
  # Add new columns
  mutate(
    treatment_age = treatment_data-as.numeric(birth_date)
  ) %>%
  # Change to categories
  mutate(across(
    .cols = -c(age, treatment_age, weight, thickness_tumor, birth_date),
    as.factor)
  ) %>%
  # Filter out men
  filter(
    gender == 0
  ) %>%
  # Remove singular columns
  select_if(function(col) length(unique(col)) > 1)
  


# Write data ---------------------------------------------------------------

saveRDS(aug_data, file = "data/03_clean_augmented_combined_breastcancer_data.rds")
write_csv(x = clean_data, 
          file = "presentation/clean_augmented_data.csv")

