# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R") # get error in zipping
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/04_analysis.R")
source(file = "R/05_analysis_ii.R")
source(file = "R/06_analysis_iii.R")
source(file = "R/07_heatmap.R")
source(file = "R/08_gif.R")
source(file = "R/09_MCA_visualisation.R")
source(file = "R/10_multinomial_log_reg_model_tumorType.R")
source(file = "R/11_multinomial_log_reg_model_condition.R")
#source(file = "R/12_PCA_analysis.R") # get error


