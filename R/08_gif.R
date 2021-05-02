# This is a plot in a gif format which shows how the age of treatment is related
# to the thickness tumors, I have also added some cultural information like 
#education and age of first time giving birth.
# We can see that the levels of education increases with the time
# Also that along the years, the patient is treated earlier
# When the person is treated earlier, the size of tumors is more diverse

# This file requires some packages and loading time to create the gif file



# Clean workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)
library(gganimate)
library(gifski)
theme_set(theme_bw()) 

# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")


# Visualise data ----------------------------------------------------------
myPlot <- my_data_clean_aug %>%
  ggplot(aes(treatment_age, thickness_tumor, size = age_FirstGivingBirth, color = education_type)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Birth year: {frame_time}', x = 'Age of treatment', y = 'Tumor thickness') +
  transition_time(birth_date) +
  ease_aes('linear')


animate(myPlot, duration = 20, fps = 20, width = 800, height = 800, renderer = gifski_renderer())
anim_save("results/08_output.gif")
# Write data --------------------------------------------------------------
anim_save("08_gif_birth_age_treatment_age.gif", path = "results/")
anim_save("08_gif_birth_age_treatment_age.gif", path = "presentation/")



