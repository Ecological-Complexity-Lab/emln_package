library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(emln)


parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/datasets"
sub.folders.short <- list.dirs(path = parent.folder, recursive = F, full.names = F)
sub.folders.long <- list.dirs(path = parent.folder, recursive = F, full.names = T)

#order the sub folders according to the id, using the short sub folders so I can use the id, but I eventually need the long to read the csvs
sub.folders.long <- sub.folders.long[str_order(sub.folders.short, numeric = T)]

files_to_check <- NULL
for (i in 1:80) {
  desc <- read_csv(paste0(sub.folders.long[i],'/description.csv'))
  if (nrow(desc[desc$attribute=='multilayer_network_type',])>1) {
    files_to_check <- c(files_to_check,i)
  }
}


net32 <- load_emln(32)
net33 <- load_emln(33)
net50 <- load_emln(50)
net51 <- load_emln(51) #look more closely -- fixed
net57 <- load_emln(57)
net58 <- load_emln(58)
net59 <- load_emln(59) #not good -- fixed
net60 <- load_emln(60)
net70 <- load_emln(70)
net73 <- load_emln(73)


