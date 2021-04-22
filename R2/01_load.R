# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(readxl)


# Load data ---------------------------------------------------------------
dead_cases <- read_excel("data/_raw/death.xlsx", na = "-")
recovered_cases <- read_excel("data/_raw/recovered.xlsx",na = "-")
under_treatment_cases <- read_excel("data/_raw/under_treatment.xlsx",na = "-")


# Write data --------------------------------------------------------------




