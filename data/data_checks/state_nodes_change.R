
nodes<-read.csv('nodes.csv')
if (T %in% str_detect(string = nodes$value,pattern = ',')) {
  need_to_check<-append(need_to_check, sub.folders[i])
  print(sub.folders[i])
}

need_to_check<-c()

parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/noa_frydman"
sub.folders <- list.dirs(parent.folder, recursive=TRUE)[-1]
for (i in 1:17) {
  setwd(sub.folders[i])
  source('~/GitHub/emln_db/EMLN_Datasets/state_nodes_change.R')
}

parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/ofir_segev"
sub.folders <- list.dirs(parent.folder, recursive=TRUE)[-1]
for (i in 1:17) {
  setwd(sub.folders[i])
  source('~/GitHub/emln_db/EMLN_Datasets/state_nodes_change.R')
}

write_csv(as.data.frame(need_to_check),'~/GitHub/emln_db/EMLN_Datasets/check_state_nodes.csv')
