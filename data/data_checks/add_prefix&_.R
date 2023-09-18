library(stringr)
###adding the serial numbers for each of the networks###

parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/datasets"

setwd("~/GitHub/emln_db/EMLN_Datasets/datasets")

sub.folders <- list.dirs(parent.folder, recursive=TRUE, full.names = F)[-1]


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




#change the numbers after deleting network 59 and 15
sub.folders <- sub.folders[order(sub.folders[as.numeric(str_extract(string = sub.folders, pattern = '^[0-9]+'))])]
for (i in 1:length(sub.folders)) {
  if (file.exists(sub.folders[i])) {
    file.rename(from = sub.folders[i], to = gsub(x = sub.folders[i], replacement = toString(i),pattern = '^[0-9]+'))
  } else {
    cat("The file does not exist")
  }
}
