# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
dead_cases <- read_csv("data/01_death_cases.csv", na = "-")
recovered_cases <- read_csv("data/01_recovered_cases.csv", na = "-")
under_treatment_cases <- read_csv("data/01_under_treatment_cases.csv", na = "-")


# Wrangle data ------------------------------------------------------------

dead_cases <- dead_cases %>%
  mutate(across(-condition, as.double))

recovered_cases <- recovered_cases %>%
  mutate(across(-condition, as.double))

under_treatment_cases <- under_treatment_cases %>%
  mutate(across(-condition, as.double))


comb_data <- dead_cases %>% 
  full_join(recovered_cases) %>%
  full_join(under_treatment_cases)




comb_clean_data <- comb_data %>%
  # Cleanup of column names
  rename_with(.cols = everything, 
              ~str_remove(string = .x, pattern = "[\r]?\n")) %>%
  rename(Birth_control = "Birth_control(Contraception)") %>%
  # Cleanup of binary variables
  mutate(across(
    .cols = c(hereditary_history, marital_status, marital_length,
              pregnency_experience, age_FirstGivingBirth, abortion, taking_heartMedicine,
              taking_blood_pressure_medicine, taking_gallbladder_disease_medicine, smoking,
              alcohol, breast_pain, radiation_history, Birth_control, Benign_malignant_cancer),
    ~ case_when(
      is.na(.) ~ NA_real_,
      . == 0   ~  0,
      TRUE     ~  1)
    )
  ) %>%
  # Cleanup of individual columns
    mutate(
      blood = ifelse(blood %in% seq(0,7), 
                     yes = blood, no = NA_real_),
      birth_date = ifelse(nchar(birth_date) == 4,
                          yes = birth_date, no = NA_real_)
    ) %>%

  # low age and low weight
  filter(age >= 20, weight >= 35) %>%
  # not got period yet, but gave birth (= does not make sense) (1 person)
  filter(!(menstrual_age == "not yet" & pregnency_experience == 0))%>%
  
  
  # Filter out men
  filter(gender == 0) 

# Remove inconsistencies in the data
# Based on UK statistics, only look at women above 20 years old
# (https://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/breast-cancer/incidence-invasive#heading-One)
# Adult women 1-percentile is 145cm or lower
# (https://dqydj.com/height-percentile-calculator-for-men-and-women/)
# Calculating BMI, a 145cm woman is underweight if weight = 35 kg, extremely if under.
# (https://getcalc.com/health-bmi-54kg-145cm.htm)
# Hence remove all with weight under 35kg, as that is unlikely (also given how few are 145cm)

# Write data --------------------------------------------------------------

write_csv(x = comb_clean_data,
          file = "data/02_clean_combined_cases.csv")


