# 
# Script creates basic distribution plots of the different variables
#

# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)

# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")


# Visualise data ----------------------------------------------------------

# Boxplot of numerical variables 
numeric_boxplot <- my_data_clean_aug %>%
                    select(-patient_id) %>%
                    select_if(is.numeric) %>%
                    pivot_longer(cols = everything()) %>% # could also use gather(), then name would be called key instead
                    ggplot(aes(value)) +
                    facet_wrap(~ name, 
                               scales = "free") +
                    coord_flip() +
                    geom_boxplot() + 
                    ggtitle("Boxplots of numerical values") + 
                    theme(plot.title = element_text(size=15, 
                                                    hjust = 0.5))

# Histogram of numerical variables 
numeric_hist_15_bins <- my_data_clean_aug %>%
                        select(-patient_id) %>%
                        select_if(is.numeric) %>%
                        pivot_longer(cols = everything()) %>%
                        ggplot(aes(value)) +
                        facet_wrap(~ name, 
                                   scales = "free") +
                        geom_histogram(bins = 15) +
                        ggtitle("Distribution of numerical values - 15 bins") + 
                        theme(plot.title = element_text(size=15, 
                                                        hjust = 0.5))
  


# Barchart of categorical variables 
# Half the variables
categorical_bar_part1 <- my_data_clean_aug %>%
                          select_if(is.factor) %>%
                          select(education_type:taking_heartMedicine, 
                                 -id_healthcenter) %>%
                          pivot_longer(cols = everything()) %>%
                          ggplot(aes(value)) +
                          facet_wrap(~ name, 
                                     scales = "free") +
                          geom_bar() + 
                          ggtitle("Distribution of categorical values (part 1)") + 
                          theme(axis.text.x = element_text(angle = 45, 
                                                           size=9, 
                                                           hjust=1, 
                                                           vjust = 1), 
                                axis.text.y= element_text(size=8, 
                                                          hjust=1, 
                                                          vjust = 1),
                                plot.title = element_text(size=15, 
                                                          hjust = 0.5)
                          ) 

# The other half of the variables
categorical_bar_part2 <- my_data_clean_aug %>%
                          select_if(is.factor) %>%
                          select(taking_gallbladder_disease_medicine:condition, 
                                 -id_healthcenter) %>%
                          pivot_longer(cols = everything()) %>%
                          ggplot(aes(value)) +
                          facet_wrap(~ name, 
                                     scales = "free") +
                          geom_bar() + 
                          ggtitle("Distribution of categorical values (part 2)") + 
                          theme(axis.text.x = element_text(angle = 45, 
                                                           size=9, 
                                                           hjust=1, 
                                                           vjust = 1), 
                                axis.text.y= element_text(size=8, 
                                                          hjust=1, 
                                                          vjust = 1),
                                plot.title = element_text(size=15, 
                                                          hjust = 0.5)
                          ) 

# Write data --------------------------------------------------------------
ggsave(
  "04_analysis_i_boxplot_numeric_variables.png",
  plot = numeric_boxplot,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 20,
  height = 16,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_histogram_15bins_numeric_variables.png",
  plot = numeric_hist_15_bins,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 20,
  height = 14,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_barchart_categorical_education_to_heartmedicine.png",
  plot = categorical_bar_part1,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 20,
  height = 16,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_barchart_categorical_gallmedicine_to_condition.png",
  plot = categorical_bar_part2,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 25,
  height = 16,
  units = "cm",
  dpi = 500
)

