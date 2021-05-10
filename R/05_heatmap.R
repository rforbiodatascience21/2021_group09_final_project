# Heatmap to show how the variables correlated 

# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)

# Load data ---------------------------------------------------------------
clean_data <- read_csv(file = "data/02_clean_combined_cases.csv")

# Subset data ---------------------------------------------------------------
# Subset of numeric and ordinal variables
subset_numeric <- clean_data %>% 
  mutate(treatment_age = treatment_data - as.numeric(birth_date)) %>% 
  select(treatment_age, education, birth_date, age, weight, thickness_tumor, giving_birth, menstrual_age, menopausal_age) %>% 
  mutate(across(.cols = everything(), as.numeric))

# Subset of binary variables
subset_binary<- clean_data %>% 
  select(hereditary_history, marital_status, pregnency_experience, age_FirstGivingBirth,
         abortion, taking_heartMedicine, taking_blood_pressure_medicine,
         taking_gallbladder_disease_medicine, smoking, alcohol, breast_pain,
         radiation_history, Birth_control, Benign_malignant_cancer) %>% 
  mutate(across(.cols = everything(), as.numeric))

# Correlation Matrix ---------------------------------------------------------------

corr_numeric <- subset_numeric %>% 
  select(where(is.numeric)) %>%
  cor(x=., use="pairwise.complete.obs", method= "spearman") %>% 
  round(digits = 5) %>% 
  as_tibble(rownames = "Var1")

corr_numeric_longer <- corr_numeric %>% 
  pivot_longer(cols = -Var1,names_to = "Var2", values_to = "corr")

corr_binary <- subset_binary %>% 
  select(where(is.numeric)) %>%
  cor(x=., use="pairwise.complete.obs", method= "spearman") %>% 
  round(digits = 5) %>% 
  as_tibble(rownames = "Var1")

corr_binary_longer <- corr_binary %>% 
  pivot_longer(cols = -Var1,names_to = "Var2", values_to = "corr")

# Heatmap of correlation matrix ---------------------------------------------------------------

corr_numeric_heatmap <- corr_numeric_longer %>% 
  ggplot(aes(x = Var1, y = Var2, fill = corr)) + 
  geom_tile()+
  ggtitle("Correlation of numeric and ordinal variables") + 
  labs(y = "", x = "")+
  theme_minimal(base_family = "Avenir") +
  theme(axis.text.x = element_text(angle = 45, 
                                   size = 6, 
                                   hjust = 1, 
                                   vjust = 1), 
        axis.text.y= element_text(size = 6, 
                                  hjust = 1, 
                                  vjust = 1),
        plot.title = element_text(size = 9, 
                                  hjust = 0.5),
        legend.title=element_blank(),
        legend.text=element_text(size = 6)) +
  scale_fill_gradient2(low = "#00BFC4",
                       mid = "azure2",
                       high = "#F8766D",
                       midpoint = 0)

corr_binary_heatmap <- corr_binary_longer %>% 
  ggplot(aes(x=Var1, y=Var2, fill=corr)) + 
  geom_tile()+
  ggtitle("Correlation of binary variables") + 
  labs(y = "", x = "")+
  theme_minimal(base_family = "Avenir")+
  theme(axis.text.x = element_text(angle = 60, 
                                   size = 5, 
                                   hjust = 1, 
                                   vjust = 1), 
        axis.text.y= element_text(size = 5, 
                                  hjust = 1, 
                                  vjust = 1),
        plot.title = element_text(size = 8, 
                                  hjust = 0.5),
        legend.title=element_blank(),
        legend.text=element_text(size = 4)) +
  scale_fill_gradient2(low = "#00BFC4",
                       mid = "azure2",
                       high = "#F8766D",
                       midpoint = 0)

# Save plots --------------------------------------------------------------
ggsave(
  "05_heatmap_numeric.png",
  plot = corr_numeric_heatmap,
  path = "results/",
  device = "png",
  scale = 1,
  width = 10,
  height = 8,
  units = "cm",
  dpi = 500
)

ggsave(
  "05_heatmap_binary.png",
  plot = corr_binary_heatmap,
  path = "results/",
  device = "png",
  scale = 1,
  width = 11,
  height = 8,
  units = "cm",
  dpi = 500
)

