#
# Visualizations of data
#
#

# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork) #remember to install

# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")


## Densitogram function on this data
densitogram_plot <- function(col_name, x_label_str, stratify_col, legend_str) {
  plot <- my_data_clean_aug %>%
    filter(condition != "under treatment") %>%
    ggplot(mapping = aes(x = {{col_name}}, 
                         fill = {{stratify_col}})) +
    geom_density(alpha=0.5) +
    labs(x = x_label_str, 
         fill = legend_str) +
    theme_minimal(base_family = "Avenir") 
  
  return(plot)
}




# Visualise data ----------------------------------------------------------

### Densitogram on condition ###
p_treatment_age_con <- densitogram_plot(treatment_age, 
                                    "Age at Treatment",
                                    condition,
                                    "Condition")
p_weight_con <- densitogram_plot(weight, 
                             "Weight",
                             condition,
                             "Condition") 
p_thickness_tumor_con <- densitogram_plot(thickness_tumor, 
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
p_treatment_age_tumor <- densitogram_plot(treatment_age, 
                                        "Age at Treatment",
                                        Benign_malignant_cancer,
                                        "Tumor Type")
p_weight_tumor <- densitogram_plot(weight, 
                                 "Weight",
                                 Benign_malignant_cancer,
                                 "Tumor Type") 
p_thickness_tumor_tumor <- densitogram_plot(thickness_tumor, 
                                          "Thickness of Tumor",
                                          Benign_malignant_cancer,
                                          "Tumor Type") 



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
  "04_analysis_ii_densitograms_on_condition.png",
  plot = densitogram_condition,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 17,
  height = 12,
  units = "cm",
  dpi = 500)

ggsave(
  "04_analysis_ii_densitograms_on_tumor.png",
  plot = densitogram_tumor,
  path = "results/plots/",
  device = "png",
  scale = 1,
  width = 17,
  height = 12,
  units = "cm",
  dpi = 500)
