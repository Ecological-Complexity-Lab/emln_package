parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/noa_frydman/"
sub.folders <- list.dirs(parent.folder, recursive=TRUE)[-1]
r.scripts <- '~/GitHub/emln_db/EMLN_Datasets/ofir_segev/temporal_networks.R'
check<-NULL

# Run scripts in sub-folders 
for(i in sub.folders) {
  if (str_detect(i,pattern = 'Temporal')){
  setwd(i)
  source(r.scripts)} else {
    next
  }
}

write.csv(check,'temporal_networks.csv')
