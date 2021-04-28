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
corr <- clean_data %>% 
  select(where(is.numeric)) %>%
  select(-gender, -treatment_data) %>%
  cor() %>% 
  round(digits = 2) %>% 
  as_tibble(rownames = "Var1")

corr_longer <- corr %>% pivot_longer(cols = -Var1,names_to = "Var2", values_to = "corr")

corr_heatmap <- corr_longer %>% 
  ggplot(aes(x=Var1, y=Var2, fill=corr)) + 
  geom_tile()+
  ggtitle("Heatmap - Correlation of numeric variables") + 
  labs(y="",x="")+
  theme(axis.text.x = element_text(angle = 45, 
                                   size=9, 
                                   hjust=1, 
                                   vjust = 1), 
        axis.text.y= element_text(size=9, 
                                  hjust=1, 
                                  vjust = 1),
        plot.title = element_text(size=15, 
                                  hjust = 0.5),
        legend.title=element_blank()
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

