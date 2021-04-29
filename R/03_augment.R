# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
dead_cases <- read_csv(file = "data/02_clean_dead_cases.csv")
recovered_cases <- read_csv(file = "data/02_clean_recovered_cases.csv")
under_treatment_cases <- read_csv(file = "data/02_clean_under_treatment_cases.csv")


# Wrangle data ---------------------------------------------------------------

# Join data sets on all columns (as all columns are identical)
dead_cases<- dead_cases %>%
  mutate(birth_date=as.double(birth_date))

clean_data <- dead_cases %>% 
  full_join(recovered_cases) %>%
  full_join(under_treatment_cases) %>%
  rename(Birth_control = 'Birth_control(Contraception)') %>%
  #There is 0,1,2 and should be 0,1
  mutate(Benign_malignant_cancer = ifelse(Benign_malignant_cancer==0, yes = 0, no = 1),
         radiation_history = ifelse(radiation_history==0, yes = 0, no = 1),
         hereditary_history = ifelse(hereditary_history==0, yes = 0, no = 1),
         breast_pain = ifelse(breast_pain==0, yes = 0, no = 1)
         ) %>% 
  # Blood 44 has no meaning and cannot be assigned to a correct category:
  filter(blood != 44) %>%
  # Add names to categories
  mutate("education_type" = case_when(education == 0 ~ "Illiterate",
                                    education == 1 ~ "Elementary",
                                    education == 2 ~ "Middle School",
                                    education == 3 ~ "High School",
                                    education == 4 ~ "Diploma",
                                    education == 5 ~ "Associate",
                                    education == 6 ~ "Bachelor",
                                    education == 7 ~ "Master"),
         .after = education)%>%
  mutate("blood_type" = case_when(blood == 0 ~ "A+",
                                  blood == 1 ~ "A-",
                                  blood == 2 ~ "AB+",
                                  blood == 3 ~ "AB-",
                                  blood == 4 ~ "B+",
                                  blood == 5 ~ "B-",
                                  blood == 6 ~ "O+",
                                  blood == 7 ~ "O-"),
         .after = blood) %>%
  mutate("age_first_birth" = case_when(age_FirstGivingBirth == 0 ~ "under 30",
                                       age_FirstGivingBirth == 1 ~ "above 30"),
         .after = age_FirstGivingBirth) %>%
  mutate("Menstrual_Age" = case_when(menstrual_age == 0 ~ "not yet",
                                     menstrual_age == 1 ~ "under 12",
                                     menstrual_age == 2 ~ "above 12"),
         .after = menstrual_age) %>%
  mutate ("Menopausal_Age" = case_when(menopausal_age == 0 ~ "not yet",
                                       menopausal_age == 1 ~ "under 50",
                                       menopausal_age == 2 ~ "above 50"),
          .after =menopausal_age) %>%
  mutate ("type_cancer" = case_when(Benign_malignant_cancer == 0 ~ "Benign",
                                    Benign_malignant_cancer == 1 ~ "Malignant"),
          .after = Benign_malignant_cancer) %>%
  filter(gender == 0) %>%

  select(-gender, -treatment_data) %>%
  # Change to categories
  mutate(education_type = as.factor(education_type),
         blood_type = as.factor(blood_type),
         hereditary_history = as.factor(hereditary_history),
         marital_status = as.factor(marital_status),
         marital_length = as.factor(marital_length),
         pregnency_experience = as.factor(pregnency_experience),
         giving_birth = as.factor(giving_birth),
         age_FirstGivingBirth = as.factor(age_FirstGivingBirth),
         age_first_birth = as.factor(age_first_birth),
         abortion = as.factor(abortion),
         taking_blood_pressure_medicine = as.factor(taking_blood_pressure_medicine),
         taking_gallbladder_disease_medicine = as.factor(taking_gallbladder_disease_medicine),
         smoking = as.factor(smoking),
         taking_heartMedicine = as.factor(taking_heartMedicine),
         alcohol = as.factor(alcohol),
         breast_pain = as.factor(breast_pain),
         radiation_history = as.factor(radiation_history),
         Birth_control = as.factor(Birth_control),
         menstrual_age = as.factor(menstrual_age),
         menopausal_age = as.factor(menopausal_age),
         Benign_malignant_cancer = as.factor(Benign_malignant_cancer),
         type_cancer = as.factor(type_cancer),
         blood = as.factor(blood),
         education = as.factor(education),
         id_healthcenter = as.factor(id_healthcenter),
         patient_id = as.factor(patient_id),
         id_treatment_region = as.factor(id_treatment_region),
         id_healthcenter = as.factor(id_healthcenter), 
         condition = as.factor(condition)
         )

# View data and colum
#View(clean_data)

# Write data ---------------------------------------------------------------

saveRDS(clean_data, file = "data/03_clean_augmented_combined_breastcancer_data.rds")

