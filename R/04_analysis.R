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
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

my_data_clean_aug %>%
  filter(age >20, weight< 40)%>%
  count()
  

my_data_clean_aug %>%
  keep(as.factor) %>%
  #gather() %>%
  #pivot_wider(keep(as.factor), names_from="value", values_from="key")
  #distinct() %>%

  group_by(key) %>%
  count(.)
  #ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()



# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
