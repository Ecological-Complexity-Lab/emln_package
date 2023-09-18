setwd('~')
x <- list.dirs('./GitHub/emln_db/EMLN_Datasets/datasets/', full.names = F, recursive = F)
sub.folders.short <- list.dirs(path = './GitHub/emln_db/EMLN_Datasets/datasets', recursive = F, full.names = F)
sub.folders.long <- list.dirs(path = './GitHub/emln_db/EMLN_Datasets/datasets', recursive = F, full.names = T)

folders_new_name_db <- gsub(sub.folders.long,pattern = '(p|P)ertubation',replacement = 'Perturbation')
file.rename(from = sub.folders.long, to = folders_new_name)


descriptions_to_change <- c(19,50,51,74)
