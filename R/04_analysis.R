# Script creates basic distribution plots of the different variables
#### Boxplot of all numerical variables in one plot
#### Histogram of all numerical variables in one plot
#### Barchart of all categorical variables in one plot
#### Boxplots of 1 variable, stratified on another
#### Countplots of 1 variable, stratified on another
#### Densitograms stratified on condition
#### Histograms of tumorthickness stratified on medcinies

# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)

# Get functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")


# Wrangle data ---------------------------------------------------------------
# Only look at samples with condition = dead or = recovered
data_condition_dead_recovered <- my_data_clean_aug %>%
  filter(condition != "under treatment")


# Visualise data ----------------------------------------------------------
#### Overall distribution plots ####
# Common boxplot of numerical variables 
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
                    theme_minimal(base_family = "Avenir")+
                    theme(plot.title = element_text(size=15, 
                                                    hjust = 0.5))


# Common histogram of numerical variables 
numeric_hist_15_bins <- my_data_clean_aug %>%
                        select(-patient_id) %>%
                        select_if(is.numeric) %>%
                        pivot_longer(cols = everything()) %>%
                        ggplot(aes(value)) +
                        facet_wrap(~ name, 
                                   scales = "free") +
                        geom_histogram(bins = 15) +
                        ggtitle("Distribution of numerical values - 15 bins") +
                        theme_minimal(base_family = "Avenir") +
                        theme(plot.title = element_text(size=15, 
                                                        hjust = 0.5))
  

# Common barchart of categorical variables 
# Half the variables
categorical_bar_part1 <- my_data_clean_aug %>%
                          select_if(is.factor) %>%
                          select(education:taking_heartMedicine, 
                                 -id_healthcenter, -blood) %>%
                          pivot_longer(cols = everything()) %>%
                          ggplot(aes(value)) +
                          facet_wrap(~ name, 
                                     scales = "free") +
                          geom_bar() + 
                          ggtitle("Distribution of categorical values (part 1)") + 
                          theme_minimal(base_family = "Avenir") +
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

# Common barchart of the other half of the categorical variables
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
                          theme_minimal(base_family = "Avenir") +
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
box_age_condition <- box_plot(data_condition_dead_recovered, 
                              age, 
                              "Age", 
                              condition, 
                              "Condition")

violin_age_condition <- violin_plot(data_condition_dead_recovered, 
                                    age, 
                                    "Age", 
                                    condition, 
                                    "Condition")

box_tumor_condition <- box_plot(data_condition_dead_recovered, 
                                thickness_tumor, 
                                "Tumor thickness", 
                                condition, 
                                "Condition")

violin_tumor_condition <- violin_plot(data_condition_dead_recovered, 
                                      thickness_tumor, 
                                      "Tumor thickness", 
                                      condition, 
                                      "Condition")

box_weight_condition <- box_plot(data_condition_dead_recovered, 
                                weight, 
                                "Weight", 
                                condition, 
                                "Condition")

violin_weight_condition <- violin_plot(data_condition_dead_recovered, 
                                      weight, 
                                      "Weight", 
                                      condition, 
                                      "Condition")


count_smoking_condition <- count_plot(data_condition_dead_recovered, 
                                      smoking, 
                                      "Smoking", 
                                      condition, 
                                      "Condition")

count_alcohol_condition <- count_plot(data_condition_dead_recovered, 
                                      alcohol, 
                                      "Alcohol", 
                                      condition, 
                                      "Condition")

count_radiation_condition <- count_plot(data_condition_dead_recovered, 
                                        radiation_history, 
                                        "Radiation History",
                                       condition, 
                                       "Condition")

count_blood_condition <- count_plot(data_condition_dead_recovered, 
                                    blood, 
                                    "Blood Type",
                                    condition, 
                                    "Condition")

count_brestpain_condition <- count_plot(data_condition_dead_recovered, 
                                  breast_pain, 
                                  "Breast Pain",
                                  condition, 
                                  "Condition")


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




### Histogram tumor thicknesses stratified on medicines ###

heart_tumor <- hist_plot(my_data_clean_aug, 
                   thickness_tumor, 
                   "Tumor thickness", 
                   taking_heartMedicine, "
                   Taking heart medicine")

blood_tumor <- count_plot(my_data_clean_aug, 
                   thickness_tumor, 
                   "Tumor thickness", 
                   taking_blood_pressure_medicine, 
                   "Taking blood pressure medicine")

gallbladdar_tumor <- count_plot(my_data_clean_aug, 
                               thickness_tumor, 
                               "Tumor thickness", 
                               taking_gallbladder_disease_medicine, 
                               "Taking gallbladder medicine")

radiation_tumor <- count_plot(my_data_clean_aug, 
                             thickness_tumor, 
                             "Tumor thickness", 
                             radiation_history, 
                             "Radiation history")

###Combine plost######
medicine_hist <- heart_tumor + blood_tumor + gallbladdar_tumor + radiation_tumor +
  plot_annotation(title = 'Distribution of tumor thickness stratified on:')





# Write data --------------------------------------------------------------
### Overall distribution plots ###
ggsave(
  "04_analysis_boxplot_numeric_variables.png",
  plot = numeric_boxplot,
  path = "results/",
  device = "png",
  scale = 1,
  width = 20,
  height = 16,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_histogram_15bins_numeric_variables.png",
  plot = numeric_hist_15_bins,
  path = "results/",
  device = "png",
  scale = 1,
  width = 20,
  height = 16,
  units = "cm",
  dpi = 500
)


ggsave(
  "04_analysis_barchart_categorical_education_to_heartmedicine.png",
  plot = categorical_bar_part1,
  path = "results/",
  device = "png",
  scale = 1,
  width = 25,
  height = 18,
  units = "cm",
  dpi = 500
)


ggsave(
  "04_analysis_barchart_categorical_gallmedicine_to_condition.png",
  plot = categorical_bar_part2,
  path = "results/",
  device = "png",
  scale = 1,
  width = 25,
  height = 18,
  units = "cm",
  dpi = 500
)


### Single plots stratified###
ggsave(
  "04_analysis_box_age_on_condition.png",
  plot = box_age_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_box_tumor_on_condition.png",
  plot = box_tumor_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_box_weight_on_condition.png",
  plot = box_weight_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_violin_tumor_on_condition.png",
  plot = violin_tumor_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_violin_age_on_condition.png",
  plot = violin_age_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_violin_weight_on_condition.png",
  plot = violin_weight_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)


ggsave(
  "04_analysis_count_smoking_on_condition.png",
  plot = count_smoking_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)


ggsave(
  "04_analysis_count_radiation_on_condition.png",
  plot = count_radiation_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)


ggsave(
  "04_analysis_count_alcohol_on_condition.png",
  plot = count_alcohol_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_count_brestpain_on_condition.png",
  plot = count_brestpain_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)

ggsave(
  "04_analysis_count_blood_on_condition.png",
  plot = count_blood_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 500
)


### Densitograms
ggsave(
  "04_analysis_densitograms_on_condition.png",
  plot = densitogram_condition,
  path = "results/",
  device = "png",
  scale = 1,
  width = 17,
  height = 12,
  units = "cm",
  dpi = 500)

ggsave(
  "04_analysis_densitograms_on_tumor.png",
  plot = densitogram_tumor,
  path = "results/",
  device = "png",
  scale = 1,
  width = 17,
  height = 12,
  units = "cm",
  dpi = 500)


# medicine histograms stratified###3
ggsave(
  "04_analysis_histogram_medicine_tumor.png",
  plot = medicine_hist,
  path = "results/",
  device = "png",
  scale = 1,
  width = 20,
  height = 16,
  units = "cm",
  dpi = 500
)


# Show cols with NAs
#my_data_clean_aug %>% 
#  select_if(function(col) any(is.na(col))) 
