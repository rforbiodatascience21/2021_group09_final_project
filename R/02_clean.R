# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
species_data <- read_csv(file = "data/_raw/01_SPE_pitlatrine.csv")
environment_data <- read_csv(file = "data/_raw/01_ENV_pitlatrine.csv")


# Wrangle data ------------------------------------------------------------
clean_data <- species_data %>% 
  # transpose species data
  pivot_longer(cols = -Taxa,
               names_to = "Samples",
               values_to = "OTU") %>%
  # concatenate with environemental data
  full_join(environment_data, by = "Samples") %>%
  relocate(Samples, .before = Taxa) %>%
  # add sample location column
  mutate(Location = case_when(str_detect(Samples, "T") ~ "Tanzania",
                              str_detect(Samples, "V") ~ "Vietnam"))


# Write data --------------------------------------------------------------
write_csv(x = clean_data,
          file = "data/02_clean_data.csv")


