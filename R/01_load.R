
# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")


# Load data ---------------------------------------------------------------
# Temporary directory
temp_dir = "data/temp"
dir.create(temp_dir)

# Unzip files
path_zipped = "data/_raw/breastcancer_kaggle.zip"
unzip(path_zipped, 
      exdir = temp_dir)

# Rename data names
files <- list.files(path = temp_dir)
data_names <- map_chr(files, 
                      ~str_remove (string = .x, 
                                   ".xlsx")) %>%
  map_chr(~str_replace (.x, 
                        " ", 
                        "_"))
# Read excel files
data_dfs <- map(str_c(temp_dir, 
                      files, 
                      sep = "/"), 
                ~ read_excel(path = .x, 
                             na = "-"))
# Delete temp
unlink(temp_dir, recursive = TRUE) 

# Write data --------------------------------------------------------------

file_names = str_c("01_", data_names, "_cases", ".csv", sep="")
file_paths = str_c("data", file_names, sep="/")

map2(data_dfs, file_paths, ~write_csv(x = .x, file = .y))

