
x <- list.dirs('./GitHub/emln_db/EMLN_Datasets/datasets/', full.names = F, recursive = F)
sub.folders.short <- list.dirs(path = './GitHub/emln_db/EMLN_Datasets/datasets/', recursive = F, full.names = F)
sub.folders.long <- list.dirs(path = './GitHub/emln_db/EMLN_Datasets/datasets/', recursive = F, full.names = T)

#order the sub folders according to the id, using the short sub folders so I can use the id, but I eventually need the long to read the csvs
sub.folders.long <- sub.folders.long[str_order(sub.folders.short, numeric = T)]
sub.folders.short <- sub.folders.short[str_order(sub.folders.short, numeric = T)]


for (i in 1:81) {
  dir.create(path = paste0("./GitHub/emln_package/data-raw/", sub.folders.short[i]))
}





######
for (i in 1:81) {
  list_of_files <- NULL
  current_folder <- paste0("./GitHub/emln_db/EMLN_Datasets/datasets/",sub.folders.short[i])
  new_folder <- paste0("./GitHub/emln_package/data-raw/",sub.folders.short[i])
  list_of_files <- list.files(current_folder, pattern =  "state_nodes.csv|interactions.csv|nodes.csv|layers.csv|references.csv|description.csv")
  file.copy(file.path(current_folder,list_of_files), new_folder, overwrite = T)

}

