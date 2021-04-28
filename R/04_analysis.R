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

View(my_data_clean_aug)

# Basic statistics
my_data_clean_aug %>%
  keep(is.factor) %>%
  gather(key = "key",
         value = "value",
         na.rm = TRUE, 
         #convert = TRUE,
         factor_key= TRUE) #%>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

  View(my_data_clean_aug)

# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
