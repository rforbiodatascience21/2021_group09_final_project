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
heatmap_subset <- clean_data %>% 
  mutate(treatment_age = treatment_data-as.numeric(birth_date)) %>% 
  select(treatment_age, education,birth_date,age,weight, thickness_tumor,giving_birth,menstrual_age,menopausal_age) %>% 
  mutate(across(.cols = everything(), as.numeric))
  
  
 # mutate(education = as.numeric(education),
  #       marital_length = as.numeric(marital_length),
   #      giving_birth = as.numeric(giving_birth),
    #     age_FirstGivingBirth = as.numeric(age_FirstGivingBirth),
     #    menstrual_age = as.numeric(menstrual_age),
    #     menopausal_age = as.numeric(menopausal_age),
     #    birth_date = as.numeric(birth_date)
      #   )

corr <- heatmap_subset %>% 
  select(where(is.numeric)) %>%
  cor(x=.,use="pairwise.complete.obs") %>% 
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
  "05_heatmap.png",
  plot = corr_heatmap,
  path = "results/",
  device = "png",
  scale = 1,
  width = 10,
  height = 8,
  units = "cm",
  dpi = 500
)

