# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
dead_cases <- read_csv("data//01_dead_cases.csv", na = "-")
recovered_cases <- read_csv("data//01_recovered_cases.csv",na = "-")
under_treatment_cases <- read_csv("data//01_under_treatment_cases.csv",na = "-")


# Wrangle data ------------------------------------------------------------

dead_cases <- dead_cases %>%
  mutate(treatment_age = treatment_data-as.numeric(birth_date))

recovered_cases <- recovered_cases %>%
  mutate(treatment_age = treatment_data-as.numeric(birth_date))

under_treatment_cases <- under_treatment_cases %>%
  mutate(treatment_age = treatment_data-as.numeric(birth_date))

#Remove cariage return, newline from names (\r\n)

# Write data --------------------------------------------------------------
write_csv(x = dead_cases, 
          file = "data/02_clean_dead_cases.csv")

write_csv(x = recovered_cases, 
          file = "data/02_clean_recovered_cases.csv")

write_csv(x = under_treatment_cases, 
          file = "data/02_clean_under_treatment_cases.csv")

