# rmarkdown::render('averaged_plots_all_stations.Rmd', output_dir = "report")
# rmarkdown::render('groups_experiments.Rmd', output_dir = "report")
# rmarkdown::render('groups_experiments_fgp.Rmd', output_dir = "report")
# rmarkdown::render('groups_experiments_julian.Rmd', output_dir = "report")
# rmarkdown::render('groups_experiments_lake_perc.Rmd', output_dir = "report")
# rmarkdown::render('groups_experiments_length_rec.Rmd', output_dir = "report")
rmd_files_list <- list.files(path = ".", pattern = "QSBSintegral.Rmd")
# library("purrr")
# map(.x= rmd_files_list, .f = print )
# # map(.x = rmd_files_list, .f = rmarkdown::render)
# lapply(rmd_files_list, rmarkdown::render)

for (rmd_file in rmd_files_list) {
  rmarkdown::render(rmd_file, output_dir = "report")
}

