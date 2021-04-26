# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)


# Define functions --------------------------------------------------------



# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")

# Basic statistics
my_data_clean_aug %>%
  select_if(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  stat_count()
  


# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
