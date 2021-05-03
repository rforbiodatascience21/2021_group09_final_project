# Visualizations of data
#### Densitograms stratified on one variable

# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork) 


# Get functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")


# Wrangle data ----------------------------------------------------------
# Only look at samples with condition = dead or recovered
data_condition_dead_recovered <- my_data_clean_aug %>%
  filter(condition != "under treatment")


# Visualise data ----------------------------------------------------------

### Densitogram on condition ###
p_treatment_age_con <- densitogram_plot(data_condition_dead_recovered,
                                        treatment_age,
                                        "Age at Treatment",
                                        condition,
                                        "Condition")

p_weight_con <- densitogram_plot(data_condition_dead_recovered,
                                 weight, 
                                 "Weight",
                                 condition,
                                 "Condition") 

p_thickness_tumor_con <- densitogram_plot(data_condition_dead_recovered,
                                          thickness_tumor, 
                                          "Thickness of Tumor",
                                          condition,
                                          "Condition") 

# Combine plots
densitogram_condition <- (p_treatment_age_con) / (p_weight_con | p_thickness_tumor_con) + 
  plot_annotation(title = "Differences in Treatment Age, Weight and Tumor Thickness", 
                  subtitle = "Densitograms stratified by Condition") + 
  plot_layout(guides = "collect") & # common legends
  ylab("") &
  theme(legend.position = "bottom", 
        axis.text.x= element_text(size=9, 
                                  hjust=1, 
                                  vjust = 1), 
        axis.text.y= element_text(size=9, 
                                  hjust=1, 
                                  vjust = 1),
        legend.text = element_text(size=9.5), 
        axis.title= element_text(size=11, 
                                 hjust=0.5, 
                                 vjust = 1), 
        plot.title = element_text(size=13, 
                                  hjust = 0.5),
        plot.subtitle = element_text(size=11, 
                                     hjust = 0.5)
  )


### Densitogram on tumor type ###
p_treatment_age_tumor <- densitogram_plot(data_condition_dead_recovered,
                                          treatment_age, 
                                          "Age at Treatment",
                                          Benign_malignant_cancer,
                                          "Tumor Type")

p_weight_tumor <- densitogram_plot(data_condition_dead_recovered,
                                   weight, 
                                   "Weight",
                                   Benign_malignant_cancer,
                                   "Tumor Type") 

p_thickness_tumor_tumor <- densitogram_plot(data_condition_dead_recovered,
                                            thickness_tumor, 
                                            "Thickness of Tumor",
                                            Benign_malignant_cancer,
                                            "Tumor Type") 


# Combine plots
densitogram_tumor <- (p_treatment_age_tumor) / (p_weight_tumor | p_thickness_tumor_tumor) + 
  plot_annotation(title = "Differences in Treatment Age, Weight and Tumor Thickness", 
                  subtitle = "Densitograms stratified by Tumor Type") + 
  plot_layout(guides = "collect") & # common legends
  ylab("") &
  theme(legend.position = "bottom", 
        axis.text.x= element_text(size=9, 
                                  hjust=1, 
                                  vjust = 1), 
        axis.text.y= element_text(size=9, 
                                  hjust=1, 
                                  vjust = 1),
        legend.text = element_text(size=9.5), 
        axis.title= element_text(size=11, 
                                 hjust=0.5, 
                                 vjust = 1), 
        plot.title = element_text(size=13, 
                                  hjust = 0.5),
        plot.subtitle = element_text(size=11, 
                                     hjust = 0.5)
  )



# Write data --------------------------------------------------------------
ggsave(
  "05_analysis_ii_densitograms_on_condition.png",
  plot = densitogram_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 17,
  height = 12,
  units = "cm",
  dpi = 500)


ggsave(
  "05_analysis_ii_densitograms_on_tumor.png",
  plot = densitogram_tumor,
  path = "results/",
  device = "png",
  scale = 1,
  width = 17,
  height = 12,
  units = "cm",
  dpi = 500)
