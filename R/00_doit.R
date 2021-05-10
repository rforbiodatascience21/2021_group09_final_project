# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/04_analysis.R")
source(file = "R/05_heatmap.R")
source(file = "R/06_MCA_visualisation.R")
source(file = "R/07_multinomial_log_reg_model_tumorType.R")
source(file = "R/08_multinomial_log_reg_model_condition.R")
rmarkdown::render(input = "doc/presentation.Rmd",
                  output_dir = "doc")