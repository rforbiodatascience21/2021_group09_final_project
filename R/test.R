work_dir <- getwd()
work_dir
# html file will be written to Rmd dir
rmarkdown::render("doc/presentation.RMD", 
                  # chunks will be evaluated with this working dir
                  knit_root_dir = work_dir, 
                  # md file to be converted by pandoc will be in working dir
                  intermediates_dir = work_dir)