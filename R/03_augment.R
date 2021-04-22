# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------


# Load data ---------------------------------------------------------------
clean_data <- read_csv(file = "data/02_clean_data.csv")

View(clean_data)

# Wrangle data ------------------------------------------------------------
my_data_clean_aug <- my_data_clean # %>% ...


# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          file = "data/03_my_data_clean_aug.tsv")


