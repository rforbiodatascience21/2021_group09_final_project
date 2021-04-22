# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(readxl)


# Load data ---------------------------------------------------------------
dead_cases <- read_excel("data/_raw/death.xlsx", na = "-")
recovered_cases <- read_excel("data/_raw/recovered.xlsx", na = "-")
under_treatment_cases <- read_excel("data/_raw/under_treatment.xlsx", na = "-")


# Wrangle data ---------------------------------------------------------------

# Join data sets on all columns (as all columns are identical)
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
         .after = blood)

# Fix warning; clean_data



# Write data ---------------------------------------------------------------

write_csv(x = clean_data,
          file = "data/02_clean_breastcancer_data.csv")

