#rename multiples to multiple
parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/datasets"
sub.folders.short <- list.dirs(path = parent.folder, recursive = F, full.names = F)
sub.folders.long <- list.dirs(path = parent.folder, recursive = F, full.names = T)

#order the sub folders according to the id, using the short sub folders so I can use the id, but I eventually need the long to read the csvs
sub.folders.long <- sub.folders.long[str_order(sub.folders.short, numeric = T)]
check <- NULL
for (i in 1:length(sub.folders.long)) {
  desc <- read_csv(paste0(sub.folders.long[i],'/description.csv'))
  print(desc[desc$attribute=='ecological_network_type',]$value)
  gsub(desc[desc$attribute=='ecological_network_type',]$value, pattern = 'Multiples|multiples', replacement = 'Multiple')
  print(desc[desc$attribute=='ecological_network_type',]$value)
  if(length(desc[desc$attribute=='ecological_network_type',]$value)==0) {
  check <- c(check,sub.folders.long[i])
  }
  write_csv(desc, paste0(sub.folders.long[i],'/description.csv'))
  }


desc <- read_csv("/Users/noni/GitHub/emln_db/EMLN_Datasets/datasets/39_Spatial_Krasnov_ectoparasites/description.csv")
desc[desc$attribute=='type',]$attribute <- 'multilayer_network_type'
desc <- rbind(desc, data.frame(attribute='ecological_network_type', value='Host-Parasite'))
desc <- rbind(desc, data.frame(attribute='state_nodes', value=FALSE))


write_csv(desc, '/Users/noni/GitHub/emln_db/EMLN_Datasets/datasets/39_Spatial_Krasnov_ectoparasites/description.csv')


desc <- read_csv("/Users/noni/GitHub/emln_db/EMLN_Datasets/datasets/16_Multiplex_Petipa_1970/description.csv")
desc[desc$attribute=='multilayer_network_type',]$value <- 'Environment'


write_csv(desc, "/Users/noni/GitHub/emln_db/EMLN_Datasets/datasets/16_Multiplex_Petipa_1970/description.csv")
