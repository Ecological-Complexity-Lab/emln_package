check <- NULL
x <- read_csv('nodes.csv') 
if ('attribute' %in% colnames(x)) {
  x %>%
  select(starts_with('attribute')) %>% 
  unique() 
if (nrow(x) > 0) {
  append(x = check,values = i)
}
}

##check which file has no description
parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/ofir_segev"
sub.folders <- list.dirs(parent.folder, recursive=TRUE)[-1]
list.files(path = sub.folders[1],full.names = TRUE)
files_to_check <- NULL
for (i in 1:68) {
  x <- list.files(path = sub.folders[i],full.names = TRUE)
  if (!T %in% str_detect(x,'description.csv')) {
    files_to_check <- append(files_to_check,sub.folders[i])
  }
}

write_csv(as.data.frame(files_to_check),'~/GitHub/emln_db/EMLN_Datasets/ofir_segev/no_description.csv')

