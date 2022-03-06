cd projects/demo-fcluster # location on my mac
Rscript codes/utils.R
Rscript codes/data_setup.R
Rscript codes/f_clust.R
Rscript codes/highest_diff.R
r -e "rmarkdown::render('results.Rmd', output_file = 'results.md', output_format = 'github_document')"
