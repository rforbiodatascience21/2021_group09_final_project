# Heatmap to show how are the variables corelated 

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)

# Load data ---------------------------------------------------------------
clean_data <- read_csv(file = "data/02_clean_combined_cases.csv")

# Heatmap Correlation Plot between numeric values
heatmap_subset_numeric <- clean_data %>% 
  mutate(treatment_age = treatment_data-as.numeric(birth_date)) %>% 
  select(treatment_age, education,birth_date,age,weight, thickness_tumor,giving_birth,menstrual_age,menopausal_age) %>% 
  mutate(across(.cols = everything(), as.numeric))

# Heatmap Correlation Plot between binary values
heatmap_subset_binary<- clean_data %>% 
  select(hereditary_history,marital_status,pregnency_experience,age_FirstGivingBirth,
         abortion,taking_heartMedicine,taking_blood_pressure_medicine,
         taking_gallbladder_disease_medicine,smoking,alcohol,breast_pain,
         radiation_history,Birth_control,Benign_malignant_cancer) %>% 
  mutate(across(.cols = everything(), as.numeric))
  
corr_numeric <- heatmap_subset_numeric %>% 
  select(where(is.numeric)) %>%
  cor(x=.,use="pairwise.complete.obs",method= "spearman") %>% 
  round(digits = 2) %>% 
  as_tibble(rownames = "Var1")

corr_binary <- heatmap_subset_binary %>% 
  select(where(is.numeric)) %>%
  cor(x=.,use="pairwise.complete.obs") %>% 
  round(digits = 2) %>% 
  as_tibble(rownames = "Var1")

corr_numeric_longer <- corr_numeric %>% 
  pivot_longer(cols = -Var1,names_to = "Var2", values_to = "corr")

corr_binary_longer <- corr_binary %>% 
  pivot_longer(cols = -Var1,names_to = "Var2", values_to = "corr")

corr_numeric_heatmap <- corr_numeric_longer %>% 
  ggplot(aes(x=Var1, y=Var2, fill=corr)) + 
  geom_tile()+
  ggtitle("Heatmap - Correlation of numeric variables") + 
  labs(y="",x="")+
  theme(axis.text.x = element_text(angle = 45, 
                                   size=6, 
                                   hjust=1, 
                                   vjust = 1), 
        axis.text.y= element_text(size=6, 
                                  hjust=1, 
                                  vjust = 1),
        plot.title = element_text(size=9, 
                                  hjust = 0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=6)) +
  scale_fill_gradient2(low = "darkorange3",
                       mid = "azure2",
                       high = "darkseagreen4",
                       midpoint = 0)

corr_binary_heatmap <- corr_binary_longer %>% 
  ggplot(aes(x=Var1, y=Var2, fill=corr)) + 
  geom_tile()+
  ggtitle("Heatmap - Correlation of numeric variables") + 
  labs(y="",x="")+
  theme(axis.text.x = element_text(angle = 45, 
                                   size=6, 
                                   hjust=1, 
                                   vjust = 1), 
        axis.text.y= element_text(size=6, 
                                  hjust=1, 
                                  vjust = 1),
        plot.title = element_text(size=9, 
                                  hjust = 0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=6)) +
  scale_fill_gradient2(low = "darkorange3",
                       mid = "azure2",
                       high = "darkseagreen4",
                       midpoint = 0)

# Write data --------------------------------------------------------------
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
  width = 10,
  height = 8,
  units = "cm",
  dpi = 500
)

