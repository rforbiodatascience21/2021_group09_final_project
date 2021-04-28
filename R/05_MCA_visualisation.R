# This scirpt will apply a Multiple Correspondence Analysis (MCA) which
# is a cousin to the PCA but since MCA works on categorical data and PCA does
# not MCA is employed.
# The underlying machinery and mathmatical properties differ but PCA and MCA plots,
# may be interpreted in a similar manner.


# We are interested whether it is possible to predict whether a patient, will
# recover or not, therefor we are mostly interested in the Death/recovered group.




# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)

# Define functions --------------------------------------------------------
comb_countour_plot <- function(df, x, y, grouping){
  
  p_main <-  ggplot(df, aes_string(x = x, y = y, col=grouping )) +
    geom_point(alpha=0.4)+
    geom_density_2d()+
    theme_minimal()+
    theme(legend.position = "bottom")
  
  p_den1 <- ggplot(df, aes_string(x = x, fill = grouping ))+
    geom_density(alpha=0.4)+
    theme_void() +
    theme(legend.position = "none")
  
  p_den2 <- ggplot(df, aes_string(x = y, fill = grouping ))+
    geom_density(alpha=0.4)+
    theme_void() +
    theme(legend.position = "none")+
    coord_flip()
  
  p_den1 + plot_spacer() + p_main + p_den2 + 
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
  
  
}



# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")



# Perform MCA Analysis ----------------------------------------------------


col_of_interest = c(
  "education",
  "hereditary_history",
  "blood",
  "taking_heartMedicine",
  "taking_blood_pressure_medicine",
  "taking_gallbladder_disease_medicine",
  "smoking",
  "alcohol",
  "radiation_history",
  "Birth_control",
  "menstrual_age",
  "menopausal_age",
  "pregnency_experience",
  "abortion",
  "breast_pain"
)

mca_ana <- my_data_clean_aug %>%
  select(all_of(col_of_interest)) %>%
  MASS::mca(nf = 2)

# Unfortunately it appears that broom does not support any mca functionality.

mca_row_val <- mca_ana %>%
  pluck("rs") %>%
  as_tibble() %>%
  rename_with(.fn = ~ paste("MCA", .x, sep = "_"))

MCA_aug_df <- my_data_clean_aug %>%
  bind_cols(mca_row_val)



# Visualise Analysis ------------------------------------------------------


MCA_aug_df %>%
  comb_countour_plot(x="MCA_1", y="MCA_2", grouping="condition") %>%
  ggsave(filename = "results/05_MCA_contour_all_conditions.png")


MCA_aug_df %>%
  filter(condition != "under treatment") %>%
  comb_countour_plot(x="MCA_1", y="MCA_2", grouping="condition") %>%
  ggsave(filename = "results/05_MCA_contour_DR_conditions.png")


# NOTE one could consider plotting the column values for the MCA variables.
# This would show which categories tend to group.


