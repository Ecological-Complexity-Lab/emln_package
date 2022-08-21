###adding the serial numbers for each of the networks###

parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/datasets"

setwd("~/GitHub/emln_db/EMLN_Datasets/datasets")

sub.folders <- list.dirs(parent.folder, recursive=TRUE, full.names = F)[-1]
list.files(path = sub.folders[1],full.names = TRUE)

### creating a prefix (aka ID) for network folder names
for (i in 1:length(sub.folders)) {
  if (file.exists(sub.folders[i])) {
    file.rename(from = sub.folders[i], to = paste(toString(i),sub.folders[i], sep = '_'))
  } else {
    cat("The file does not exist")
  }
}


library(stringr)
sub.folders <- list.dirs(parent.folder, recursive=TRUE, full.names = F)[-1]

### creating an "_" seperator between the author and year published in network folder names
for (i in 1:length(sub.folders)) {
  if (file.exists(sub.folders[i])) {
    index_to_add = str_locate(string = sub.folders[i], pattern = '[:alpha:](?=\\d)')[1]
    if (NA %in% index_to_add) {
      next 
    } else {
      file.rename(from = sub.folders[i], to = paste(substr(sub.folders[i], 1, index_to_add),substr(sub.folders[i], index_to_add+1, nchar(sub.folders[i])),sep='_'))
    }
  } else {
    cat("The file does not exist")
  }
}


x