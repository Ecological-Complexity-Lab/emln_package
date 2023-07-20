devtools::install_github('Ecological-Complexity-Lab/emln', force=T)
remotes::install_github("rlesur/klippy")

setwd("/Users/shai/GitHub/ecomplab/emln_package")

# Make the website
rmarkdown::render_site(input = 'website_source')

# Remove unnecesary files
unlink('docs/', recursive = TRUE, force = FALSE)
file.copy('website_source/docs/', '.', recursive = T)
unlink('website_source/docs/', recursive = TRUE, force = FALSE)
