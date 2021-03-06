# This script will apply a Multiple Correspondence Analysis (MCA) which
# is a cousin to the PCA but since MCA works on categorical data and PCA does
# not MCA is employed.
# The underlying machinery and mathmetical properties differ but PCA and MCA plots,
# may be interpreted in a similar manner.

# We are interested whether it is possible to predict whether a patient, will
# recover or not, therefor we are mostly interested in the death/recovered group.


# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")
library("cowplot")
library("ggrepel")


# Get functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


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

# Analysis
mca_ana <- my_data_clean_aug %>%
  select(all_of(col_of_interest)) %>%
  MASS::mca(nf = 2)


# Unfortunately it appears that broom does not support any MCA functionality for extraction.
# Hence, extracting 'manually'
mca_row_val <- mca_ana %>%
  pluck("rs") %>%
  as_tibble() %>%
  rename_with(.fn = ~ str_c("MCA", 
                            .x, 
                            sep = "_"))

MCA_aug_df <- my_data_clean_aug %>%
  bind_cols(mca_row_val)


# Visualise Analysis ------------------------------------------------------
MCA_aug_df %>%
  comb_countour_plot(x = "MCA_1", 
                     y = "MCA_2", 
                     grouping = "Benign_malignant_cancer",
                     legend_str = "Tumor Type") %>%
  ggsave(filename = "results/06_MCA_contour_tumorType.png",
         width = 9,
         height = 5)


MCA_aug_df %>%
  filter(condition != "under treatment") %>%
  comb_countour_plot(x = "MCA_1", 
                     y = "MCA_2", 
                     grouping = "condition",
                     legend_str = "Condition") %>%
  ggsave(filename = "results/06_MCA_contour_conditions.png",
         width = 9,
         height = 5)


# NOTE one could consider plotting the column values for the MCA variables.
# This would show which categories tend to group.

# Include arrows describing the rotation matrix.
mca_rot <- mca_ana %>%
  pluck("cs") %>%
  as_tibble(rownames = NA) %>%
  rownames_to_column() %>%
  rename( MCA_1 = `1`, 
          MCA_2 = `2`)%>%
  filter(sqrt(MCA_1**2 + MCA_1**2) > median(sqrt(MCA_1**2 + MCA_2**2))
  )

arrow_style <- arrow(angle = 20, 
                     ends = "first", 
                     type = "closed", 
                     length = grid::unit(6, "pt")
)

p = MCA_aug_df %>%
  filter(condition != "under treatment") %>%
  ggplot(aes(x = MCA_1, 
             y = MCA_2, 
             col = condition)) +
  geom_point(alpha = 0.4) +
  geom_segment(xend = 0, 
               yend = 0, 
               data = mca_rot, 
               aes(color = NULL), 
               arrow = arrow_style) +
  geom_text_repel(data = mca_rot, 
                  aes(label = rowname, 
                      color = NULL)) +
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(title = "MCA Rotation matrix superimposed on MCA1 v. MCA2",
       subtitle = "Categories and factors with euclidian norm above the median included."
  ) 

# Write data ---------------------------------------------------------------
ggsave(p, 
       filename = "results/06_MCA_supImp_rotation.png",
       width = 8,
       height = 5)

