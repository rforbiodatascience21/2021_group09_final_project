# 
# Histograms on one variable classified according to a condition 
# (having taken some kind of medicines or treatments) that could
# increase the probabilities of having a cancer breast

# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")

# Get functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Visualise data ----------------------------------------------------------

plot1 <- hist_plot(my_data_clean_aug, thickness_tumor, "Tumor thickness", taking_heartMedicine, "Taking heart medicine")

plot2 <- hist_plot(my_data_clean_aug, thickness_tumor, "Tumor thickness", taking_blood_pressure_medicine, "Taking blood pressure medicine")

plot3 <- hist_plot(my_data_clean_aug, thickness_tumor, "Tumor thickness", taking_gallbladder_disease_medicine, "Taking gallbladder medicine")

plot4 <- hist_plot(my_data_clean_aug, thickness_tumor, "Tumor thickness", radiation_history, "Radiation history")

  
final_plot<-plot1+plot2+plot3+plot4 +
  plot_annotation(title = 'How does medicine affect tumor thickness')


# Write data --------------------------------------------------------------
ggsave(
  "06_analysis_iii_histogram_medicine_tumor.png",
  plot = final_plot,
  path = "results/",
  device = "png",
  scale = 1,
  width = 20,
  height = 16,
  units = "cm",
  dpi = 500
)




