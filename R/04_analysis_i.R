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

# Functions ----------------------------------------------------------
# Basic box plot of 1 variable, stratified on another eg condition or tumor
box_plot <- function(data, col_name, x_label_str, stratify_col, legend_str) {
  title_string <- str_c("Boxplot of", x_label_str, "stratified on", legend_str , sep = " ")
  plot <- data %>%
    ggplot(mapping = aes(x = {{col_name}}, 
                         fill = {{stratify_col}})) +
    geom_boxplot(alpha=0.5) +
    labs(x = x_label_str, 
         fill = legend_str) +
    ggtitle(title_string) +
    theme_minimal(base_family = "Avenir") 
  
  return(plot)
}

# Basic count plot of 1 variable, stratified on another eg condition or tumor
count_plot <- function(data, col_name, x_label_str, stratify_col, legend_str) {
  title_string <- str_c("Distribution of", x_label_str, "stratified on", legend_str , sep = " ")
  plot <- data %>%
    ggplot(mapping = aes(x = {{col_name}}, 
                         fill = {{stratify_col}})) +
    geom_bar(alpha=0.5, position = position_dodge(width = 0.95)) +
    labs(x = x_label_str,
         y = "Count",
         fill = legend_str) +
    ggtitle(title_string) +
    theme_minimal(base_family = "Avenir") 
  
  return(plot)
}


# Visualise data ----------------------------------------------------------
#### Overall distribution plots ####
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

###### Single plots with stratification (uses plot functions) ########
box_age_condition <- box_plot(my_data_clean_aug, 
                              age, 
                              "Age", 
                              condition, 
                              "Condition")

box_tumor_condition <- box_plot(my_data_clean_aug, 
                                thickness_tumor, 
                                "Tumor thickness", 
                                condition, 
                                "Condition")


count_smoking_condition <- count_plot(my_data_clean_aug, 
                                      smoking, 
                                      "Smoking", 
                                      condition, 
                                      "Condition")

radiation_subset <- my_data_clean_aug %>%
  filter(condition != "under treatment") # only look at dead and recovered
count_radiation_condition <- count_plot(radiation_subset, 
                                        radiation_history, 
                                        "Radiation History",
                                       condition, 
                                       "Condition")

count_heart_medicine_condition <- count_plot(my_data_clean_aug, 
                                      taking_heartMedicine, 
                                      "Taking heart medicine", 
                                      condition, 
                                      "Condition")

count_blood_pressure_medicine_condition <- count_plot(my_data_clean_aug, 
                                      taking_blood_pressure_medicine, 
                                      "Taking blood pressure medicine", 
                                      condition, 
                                      "Condition")
count_gallblader_medicine_condition <- count_plot(my_data_clean_aug, 
                                      taking_gallbladder_disease_medicine, 
                                      "Taking gallblader disease medicine", 
                                      condition, 
                                      "Condition")
count_alcohol_condition <- count_plot(my_data_clean_aug, 
                                      alcohol, 
                                      "Alcohol", 
                                      condition, 
                                      "Condition")

# Write data --------------------------------------------------------------
### Overall distribution plots ###
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

### Single plots stratified###
ggsave(
  "04_analysis_i_box_age_on_condition.png",
  plot = box_age_condition,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_box_tumor_on_condition.png",
  plot = box_tumor_condition,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_count_smoking_on_condition.png",
  plot = count_smoking_condition,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_count_radiation_on_condition.png",
  plot = count_radiation_condition,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_count_alcohol_on_condition.png",
  plot = count_alcohol_condition,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_count_heart_medicine_on_condition.png",
  plot = count_heart_medicine_condition,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_count_blood_pressure_medicine_on_condition.png",
  plot = count_blood_pressure_medicine_condition,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_i_count_gallblader_medicine_on_condition.png",
  plot = count_gallblader_medicine_condition,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)
