# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
species_data = read_csv(file = "http://userweb.eng.gla.ac.uk/umer.ijaz/bioinformatics/ecological/SPE_pitlatrine.csv")
environment_data = read_csv(file = "http://userweb.eng.gla.ac.uk/umer.ijaz/bioinformatics/ecological/ENV_pitlatrine.csv")


# Write data --------------------------------------------------------------
write_csv(x = species_data, 
          file = "data/_raw/01_SPE_pitlatrine.csv")

write_csv(x = environment_data, 
          file = "data/_raw/01_ENV_pitlatrine.csv")



