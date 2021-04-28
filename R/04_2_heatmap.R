# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)

# Load data ---------------------------------------------------------------
clean_data <- readRDS(file = "data/03_clean_augmented_combined_breastcancer_data.rds")

# Heatmap Correlation Plot between numeric values
heatmap_subset <- clean_data %>% 
  mutate(education = as.numeric(education),
         marital_length = as.numeric(marital_length),
         giving_birth = as.numeric(giving_birth),
         age_FirstGivingBirth = as.numeric(age_FirstGivingBirth),
         menstrual_age = as.numeric(menstrual_age),
         treatment_age = as.numeric(treatment_age)
         )


corr <- heatmap_subset %>% 
  select(where(is.numeric)) %>%
  cor() %>% 
  round(digits = 2) %>% 
  as_tibble(rownames = "Var1")

corr_longer <- corr %>% 
  pivot_longer(cols = -Var1,names_to = "Var2", values_to = "corr")

corr_heatmap <- corr_longer %>% 
  ggplot(aes(x=Var1, y=Var2, fill=corr)) + 
  geom_tile()+
  ggtitle("Heatmap - Correlation of numeric variables") + 
  labs(y="",x="")+
  theme(axis.text.x = element_text(angle = 45, 
                                   size=15, 
                                   hjust=1, 
                                   vjust = 1), 
        axis.text.y= element_text(size=15, 
                                  hjust=1, 
                                  vjust = 1),
        plot.title = element_text(size=18, 
                                  hjust = 0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=12)
  )

# Write data --------------------------------------------------------------
ggsave(
  "04_2_heatmap.png",
  plot = corr_heatmap,
  path = "results/plots",
  device = "png",
  scale = 1,
  width = 20,
  height = 16,
  units = "cm",
  dpi = 500
)

