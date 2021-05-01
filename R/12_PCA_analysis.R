# PCA analysis
#

# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(tidyr)
library(ggplot2)
library(broom)
library(cowplot)
library(patchwork)

# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")

# PCA fit----------------------------------------------------------------
pca_fit <- my_data_clean_aug %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE) # do PCA on scaled data

my_data_aug <- pca_fit %>%
  augment(my_data_clean_aug)

pca_fit %>%
  tidy(matrix = "rotation")

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

pca_fit %>%
  tidy(matrix = "eigenvalues")


# Visualize plots----------------------------------------------------------
# plot rotation matrix
p1<-pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal_grid(12)

p2<-pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)

custom_col_scheme <- 
  scale_color_manual(
    values = c("poor" = "#D55E00", "good" = "#0072B2")
  )

#p3<-my_data_aug %>%
  ggplot(aes(x =.fittedPC1, 
             y =.fittedPC2,
             color =Benign_malignant_cancer))+
  geom_point(size= 1.5)+
  theme_minimal()+
  custom_col_scheme+
  theme(legend.position = "bottom")

PCA_plots<- p1+ labs(subtitle = "PC coordinates") +p2 +labs(subtitle = "Variance by PC") + plot_annotation(title = "PCA analysis") + plot_layout(ncol = 2)
 #p3 + labs(subtitle = "PCA visualization")+

# Save plots ------------------------------------------------------------
ggsave(
  "12_PCA_analysis_plots.png",
  plot = PCA_plots,
  path = "results/",
  device = "png",
  scale = 1,
  width = 20,
  height = 16,
  units = "cm",
  dpi = 500
)

