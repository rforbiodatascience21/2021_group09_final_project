
# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)


# Load data ---------------------------------------------------------------
dead_cases <- read_excel("data/_raw/death.xlsx", na = "-")
recovered_cases <- read_excel("data/_raw/recovered.xlsx",na = "-")
under_treatment_cases <- read_excel("data/_raw/under_treatment.xlsx",na = "-")


# Write data --------------------------------------------------------------
write_csv(x = dead_cases, 
          file = "data/_raw/01_dead_cases.csv")

write_csv(x = recovered_cases, 
          file = "data/_raw/01_recovered_cases.csv")

write_csv(x = under_treatment_cases, 
          file = "data/_raw/01_under_treatment_cases.csv")



