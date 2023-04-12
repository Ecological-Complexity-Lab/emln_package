# get all the files from the source
file_names <- list.files('website_source/', full.names = T)
file.copy(from = file.path(file_names),
          to = file.path(getwd()))

# Make the website
rmarkdown::render_site()

# Remove unnecesary files
unlink('docs/code_examples/', recursive = TRUE, force = FALSE)
unlink('docs/data/', recursive = TRUE, force = FALSE)
unlink('docs/data-raw/', recursive = TRUE, force = FALSE)
unlink('docs/man', recursive = TRUE, force = FALSE)
unlink('docs/R', recursive = TRUE, force = FALSE)
unlink('docs/private', recursive = TRUE, force = FALSE)
unlink('docs/website_source/', recursive = TRUE, force = FALSE)
unlink('docs/DESCRIPTION', recursive = TRUE, force = FALSE)
unlink('docs/NAMESPACE', recursive = TRUE, force = FALSE)
files <- list.files('docs/', full.names = T)
rm_files <- files[grep("\\.txt$", files)]
file.remove(rm_files)
rm_files <- files[grep("\\.csv$", files)]
file.remove(rm_files)
rm_files <- files[grep("\\.xlsx$", files)]
file.remove(rm_files)

# Move all the website files to the source
file_names <- list.files()
file_names_filtered <- file_names[grep("\\.(Rmd|txt|png|csv|xlsx|yml)$", file_names)]
print(file_names_filtered)
file.remove(file_names_filtered)
