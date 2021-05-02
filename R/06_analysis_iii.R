# 
# Script creates plots of different relations
#

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


# Visualise data ----------------------------------------------------------
plot1 <- my_data_clean_aug %>%
  select(taking_heartMedicine, thickness_tumor) %>%
  ggplot(aes(x = thickness_tumor, fill = taking_heartMedicine))+
  geom_histogram(binwidth = 0.05) +
  ggtitle("Heart medicine")

plot2 <- my_data_clean_aug %>%
  select(taking_blood_pressure_medicine, thickness_tumor) %>%
  ggplot(aes(x = thickness_tumor, fill = taking_blood_pressure_medicine))+
  geom_histogram(binwidth = 0.05)+
  ggtitle("Blood pressure medicine")

plot3 <- my_data_clean_aug %>%
  select(taking_gallbladder_disease_medicine, thickness_tumor) %>%
  ggplot(aes(x = thickness_tumor, fill = taking_gallbladder_disease_medicine))+
  geom_histogram(binwidth = 0.05)+
  ggtitle("Gallblader medicine")

plot4 <- my_data_clean_aug %>%
  select(radiation_history, thickness_tumor) %>%
  ggplot(aes(x = thickness_tumor, fill = radiation_history))+
  geom_histogram(binwidth = 0.05)+
  ggtitle("Radiation history")
  
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

ggsave(
  "06_analysis_iii_histogram_medicine_tumor.png",
  plot = final_plot,
  path = "presentation/",
  device = "png",
  scale = 1,
  width = 20,
  height = 16,
  units = "cm",
  dpi = 500
)




