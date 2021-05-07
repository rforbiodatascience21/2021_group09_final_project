# Script creates basic distribution plots of the different variables
#### Boxplot of all numerical variables in one plot
#### Histogram of all numerical variables in one plot
#### Barchart of all categorical variables in one plot
#### Boxplots of 1 variable, stratified on another
#### Countplots of 1 variable, stratified on another


# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(purrr)
library(broom)
library(tidyr)
library(dplyr)
library(ggplot2)

# Get functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")

# Wrangle data ---------------------------------------------------------------
# Only look at samples with condition = dead or = recovered
data_condition_dead_recovered <- my_data_clean_aug %>%
  filter(condition != "under treatment")

View(my_data_clean_aug)

# stats of data ----------------------------------------------------------
data_condition_dead_recovered %>% 
  group_by(condition) %>% 
  summarise(min = min(age), 
            mean = mean(age),
            max = max(age),
            quant = quantile(age)) 

data_condition_dead_recovered %>% 
  group_by(condition) %>% 
  count(.)

# Look into radiation therapy, and tumor type and outcome
data_condition_dead_recovered %>% 
  group_by(radiation_history, 
           Benign_malignant_cancer,
           condition) %>%
  summarise(count = n())



names(data_condition_dead_recovered)

# Visualise data ----------------------------------------------------------
#### Overall distribution plots ####
# Common boxplot of numerical variables 
my_data_clean_aug %>%
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

# Common histogram of numerical variables 
my_data_clean_aug %>%
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


# Common barchart of categorical variables 
# Half the variables
my_data_clean_aug %>%
  select_if(is.factor) %>%
  select(education:taking_heartMedicine, 
         -id_healthcenter, -blood) %>%
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

# Common barchart of the other half of the categorical variables
my_data_clean_aug %>%
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
box_plot(data_condition_dead_recovered, 
         age, 
         "Age", 
         condition, 
         "Condition")

box_plot(data_condition_dead_recovered, 
         thickness_tumor, 
         "Tumor thickness", 
         condition, 
         "Condition")

box_plot(data_condition_dead_recovered, 
         weight,
         "Weight", 
         condition, 
         "Condition")


count_plot(data_condition_dead_recovered, 
           smoking, 
           "Smoking", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           alcohol, 
           "Alcohol", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           radiation_history, 
           "Radiation History",
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           taking_heartMedicine, 
           "Taking heart medicine", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           taking_blood_pressure_medicine, 
           "Taking blood pressure medicine", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           taking_gallbladder_disease_medicine, 
           "Taking gallblader disease medicine", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           Birth_control, 
           "Birth control", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           education, 
           "Education", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           id_treatment_region, 
           "id_treatment_region", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           hereditary_history, 
           "hereditary_history", 
           condition, 
           "Condition")


count_plot(data_condition_dead_recovered, 
           pregnency_experience, 
           "pregnency_experience", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           giving_birth, 
           "giving_birth", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           age_FirstGivingBirth, 
           "age_FirstGivingBirth", 
           condition, 
           "Condition")

count_plot(data_condition_dead_recovered, 
           abortion, 
           "abortion", 
           condition, 
           "Condition")
names(data_condition_dead_recovered)

count_plot(data_condition_dead_recovered, 
           blood, 
           "blood", 
           condition, 
           "Condition")
names(data_condition_dead_recovered)

count_plot(data_condition_dead_recovered, 
           breast_pain, 
           "breast_pain", 
           condition, 
           "Condition")
names(data_condition_dead_recovered)

count_plot(data_condition_dead_recovered, 
           menstrual_age, 
           "menstrual_age", 
           condition, 
           "Condition")
names(data_condition_dead_recovered)

count_plot(data_condition_dead_recovered, 
           Benign_malignant_cancer, 
           "Benign_malignant_cancer", 
           condition, 
           "Condition")



### One command using map function

count_plot <- function(data = my_data_clean_aug, col_name, stratify_col = "condition") {
  
  title_string <- paste("Distribution of", col_name, "stratified on", substitute(stratify_col), sep = " ")
  
  plot <- data %>%
    ggplot(mapping = aes_string(x = col_name, 
                         fill = stratify_col)) +
    geom_bar(alpha=0.5, position = position_dodge(width = 0.95)) +
    labs(x = col_name,
         y = "Count",
         fill = substitute(stratify_col)) +
    ggtitle(title_string) +
    theme_minimal(base_family = "Avenir") 
  
  #ggsave(str_c("results/TEST_04_analysis_countplot", 
  #            substitute(col_name),
  #            substitute(stratify_col), 
  #            ".png", 
  #            sep = "_"))
  
  return(plot)
}


cols_of_interest = c("Benign_malignant_cancer", "menopausal_age")

my_data_clean_aug %>%
  select(cols_of_interest) %>%
  map2(cols_of_interest, ~count_plot(col_name = .x))


####
my_data_clean_aug %>%
  #select(menstrual_age:menopausal_age) %>%
  select(cols_of_interest) %>%
  map(function(x) count_plot(col_name = x))




# Write data --------------------------------------------------------------


# Show cols with NAs
#my_data_clean_aug %>% 
#  select_if(function(col) any(is.na(col))) 
