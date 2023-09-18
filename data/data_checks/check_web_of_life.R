library(readr)
library(stringr)
library(dplyr)
library(tidyr)

parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/datasets"
sub.folders.short <- list.dirs(path = parent.folder, recursive = F, full.names = F)
sub.folders.long <- list.dirs(path = parent.folder, recursive = F, full.names = T)

#order the sub folders according to the id, using the short sub folders so I can use the id, but I eventually need the long to read the csvs
sub.folders.long <- sub.folders.long[str_order(sub.folders.short, numeric = T)]

files_to_check <- NULL
for (i in 1:80) {
  desc <- read_csv(paste0(sub.folders.long[i],'/description.csv'))
  if ('Web_of_Life' %in% desc[desc$attribute=='source',]$value) {
    interactions <- read_csv(paste0(sub.folders.long[i],'/interactions.csv'))
    if(TRUE %in% grepl(x = interactions$value, pattern = pattern_to_delete)) {
      files_to_check <- c(files_to_check,i)
    }
  }
}


# Delete all the layer names from the nodes in the networks in web --------

folders_to_change <- sub.folders.long[files_to_check]

# 6 -----------------------------------------------------------------------


interactions <- read_csv(paste0(folders_to_change[1],'/interactions.csv'))
nodes <- read_csv(paste0(folders_to_change[1],'/nodes.csv'))

node_to_check <- 'Tillandsia_sp1_M_PL_069_01'
pattern_to_delete <- '[[:punct:]][A-Z]_[A-Z][A-Z]_.*'

interactions$value <- gsub(interactions$value, pattern = pattern_to_delete, replacement = '')
nodes$value <- gsub(nodes$value, pattern = pattern_to_delete, replacement = '')

nodes <- nodes %>%
  distinct()



# 22 ----------------------------------------------------------------------


interactions <- read_csv(paste0(folders_to_change[2],'/interactions.csv'))
nodes <- read_csv(paste0(folders_to_change[2],'/nodes.csv'))
layers <- read_csv(paste0(folders_to_change[2],'/layers.csv'))

interactions$value <- gsub(interactions$value, pattern = pattern_to_delete, replacement = '')
nodes$value <- gsub(nodes$value, pattern = pattern_to_delete, replacement = '')

nodes <- nodes %>%
  distinct()

interactions <- interactions %>%
  distinct()


# 25 ----------------------------------------------------------------------
interactions <- read_csv(paste0(folders_to_change[3],'/interactions.csv'))
nodes <- read_csv(paste0(folders_to_change[3],'/nodes.csv'))
layers <- read_csv(paste0(folders_to_change[3],'/layers.csv'))
state_nodes <- read_csv(paste0(folders_to_change[3],'/state_nodes.csv'))


interactions$value <- gsub(interactions$value, pattern = pattern_to_delete, replacement = '')
nodes$value <- gsub(nodes$value, pattern = pattern_to_delete, replacement = '')

nodes <- nodes %>%
  pivot_wider(id_cols = node_id,names_from = 'attribute',values_from = 'value')

nodes <- nodes[,-1] %>%
  distinct()

nodes <- nodes %>%
  mutate(node_id=1:nrow(nodes[,-1])) %>%
  pivot_longer(!node_id, names_to = 'attribute',values_to = 'value')

is.na(nodes[nodes$attribute=='verification_level',]$value)

nodes_names[nodes$attribute=='node_name',]$value[!interactions[interactions$attribute %in% c('node_from','node_to'),]$value %in% nodes[nodes$attribute=='node_name',]$value]

nodes_names <- nodes[nodes$attribute=='node_name',]$value
