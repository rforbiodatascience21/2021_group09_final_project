
# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)


# Load data ---------------------------------------------------------------

temp_dir = "data/temp"

dir.create(temp_dir)


path_zipped = "data/_raw/breastcancer_kaggle.zip"

unzip(path_zipped, exdir = temp_dir)

files <- list.files(path = temp_dir)

data_names <- map_chr(files, ~str_remove(string = .x, ".xlsx")) %>%
                    map_chr(~str_replace(.x, " ", "_"))

data_dfs <- map(paste(temp_dir, files, sep="/"), ~ read_excel(path = .x, na ="-"))

unlink(temp_dir) 
# Write data --------------------------------------------------------------

file_names = paste("01_", data_names,"_cases", ".csv", sep="")
file_paths = paste("data/",file_names, sep="/")

map2(data_dfs, file_paths, ~write_csv(x= .x, file = .y))



