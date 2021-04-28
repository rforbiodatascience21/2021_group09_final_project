# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
dead_cases <- read_csv("data/01_death_cases.csv", na = "-")
recovered_cases <- read_csv("data/01_recovered_cases.csv",na = "-")
under_treatment_cases <- read_csv("data/01_under_treatment_cases.csv",na = "-")


# Wrangle data ------------------------------------------------------------

dead_cases <- dead_cases %>% 
  filter(gender==0 & nchar(birth_date) == 4) %>%
  mutate(treatment_age = treatment_data-as.numeric(birth_date))

recovered_cases <- recovered_cases %>% 
  filter(gender==0 & nchar(birth_date) == 4) %>%
  mutate(treatment_age = treatment_data-as.numeric(birth_date))

under_treatment_cases <- under_treatment_cases %>% 
  filter(gender==0 & nchar(birth_date) == 4) %>%
  mutate(treatment_age = treatment_data-as.numeric(birth_date))

#Remove cariage return, newline from names (\r\n)

names(dead_cases) <- map_chr(names(dead_cases), ~str_remove(string=.x, pattern = "\r\n"))
names(recovered_cases) <- map_chr(names(recovered_cases), ~str_remove(string=.x, pattern = "\r\n"))
names(under_treatment_cases) <- map_chr(names(under_treatment_cases), ~str_remove(string=.x, pattern = "\r\n"))
names(dead_cases) <- map_chr(names(dead_cases), ~str_remove(string=.x, pattern = "\n"))
names(recovered_cases) <- map_chr(names(recovered_cases), ~str_remove(string=.x, pattern = "\n"))
names(under_treatment_cases) <- map_chr(names(under_treatment_cases), ~str_remove(string=.x, pattern = "\n"))

# Write data --------------------------------------------------------------
write_csv(x = dead_cases, 
          file = "data/02_clean_dead_cases.csv")

write_csv(x = recovered_cases, 
          file = "data/02_clean_recovered_cases.csv")

write_csv(x = under_treatment_cases, 
          file = "data/02_clean_under_treatment_cases.csv")

