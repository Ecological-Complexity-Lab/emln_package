
parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/datasets"
to <- "~/GitHub/emln_db/EMLN_Datasets/datasets"
setwd("~/GitHub/emln_db/EMLN_Datasets/datasets") ##in this chunk, we want the working directory to be the one with all the networks in order to read them

sub.folders.short <- list.dirs(path = parent.folder, recursive = T, full.names = F)[-1]
sub.folders.long <- list.dirs(path = parent.folder, recursive = T, full.names = T)[-1]

#order the sub folders according to the id, using the short sub folders so I can use the id, but I eventually need the long to read the csvs
sub.folders.long <- sub.folders.long[str_order(sub.folders.short, numeric = T)]



### reading the description file, and checking if a state_nodes file exists
# then editing the description file accordingly.
for (i in 1:length(sub.folders.long)) {
  desc_temp <- read_csv(paste(sub.folders.long[i],'/description.csv',sep=''))
  if (file.exists(paste(sub.folders.long[i],'/state_nodes.csv',sep=''))) {
    desc_temp <- rbind(desc_temp, tibble(attribute = 'state_nodes', value = TRUE))
    write_csv(desc_temp, paste(sub.folders.long[i],'/description.csv',sep=''))
  } else {
    desc_temp <- rbind(desc_temp, tibble(attribute = 'state_nodes', value = FALSE))
    write_csv(desc_temp, paste(sub.folders.long[i],'/description.csv',sep=''))
  }
}
