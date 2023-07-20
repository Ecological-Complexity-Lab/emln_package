library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
references <- tibble(doi='10.1046/j.0021-8790.2001.00572.x',author='L.V. Dicks',year='05/03/2002',
                     paper_url = 'https://besjournals.onlinelibrary.wiley.com/doi/full/10.1046/j.0021-8790.2001.00572.x',
                     data_url = 'http://www.web-of-life.es/map.php?type=6')
write_csv(references, 'references.csv')

#layers
ref <- read_csv("web_references.csv")

layers <- NULL
for (i in 1:length(ref$ID)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=ref$ID[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers <- rbind(layers,tibble(layer=i,attribute='species',value=ref$Species[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='loaction',value=ref$`Locality of Studi`[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='interactions',value=ref$Interactions[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='connectance',value=ref$Connectance[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value=ref$Latitude[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value=ref$Longitude[i]))

}

write_csv(layers,'layers.csv')


#interactions
inter_table <- function (file_name, layer) {
  countryx <- read_csv(file_name)
  countryx<-column_to_rownames(countryx,var = colnames(countryx)[1])
  view(countryx)
  interaction_yearx <- matrix_to_list_bipartite(countryx,group_names = c('Pollinator','Plants'))$edge_list
  interaction_yearx <- tibble(node_from=interaction_yearx$from, layer_from= layer, node_to=interaction_yearx$to, 
                              layer_to = layer, weight=interaction_yearx$weight,type='pollination', method = 'observation')}

inter_1 <- inter_table('M_PL_006.csv', 1)
inter_2 <-  inter_table('M_PL_007.csv', 2)
read.csv('M_PL_006.csv')
interactions_test <- rbind(inter_1,inter_2)

interactions_test$node_from <- str_replace_all(interactions_test$node_from, ' ','_')
interactions_test$node_to <- str_replace_all(interactions_test$node_to, ' ','_')

interactions <- inter_long(interactions_test)

#check
test <- unique(interactions$interaction_id)

write_csv(interactions,'interactions.csv')

#nodes
M_PL_006_species <- read_csv("M_PL_006-species.csv")
M_PL_007_species <- read_csv("M_PL_007-species.csv")

all_nodes <- rbind(M_PL_006_species,M_PL_007_species)
all_nodes <- all_nodes[,-4:-6]
all_nodes <- unique(all_nodes)

species_names <- unique(all_nodes$Specie)

all_nodes$Specie <- str_replace_all(all_nodes$Specie, ' ','_')

nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes$Specie[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= all_nodes$Role[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='kingdom',value=all_nodes$Kingdom[i]))
  
}


write_csv(nodes,'nodes.csv')


#state nodes

M_PL_006_species <- add_column(layer_id = 1, node_id = NA, M_PL_006_species)
M_PL_007_species <- add_column(layer_id = 2, node_id = NA, M_PL_007_species)

state_nodes<-rbind(M_PL_006_species,M_PL_007_species)
state_nodes <- rename(state_nodes,'node_name' = 'Specie')
state_nodes$node_name %<>% str_replace_all(' ','_')

for (i in 1: nrow(state_nodes)) {
  state_nodes$node_id[i] <- filter(nodes, attribute == 'node_name', value == state_nodes$node_name[i])$node_id
}

state_nodes<-gather(state_nodes,key= attribute, value, -layer_id,-node_id) %>% 
  arrange(node_id,layer_id)
state_nodes$attribute <- str_replace_all(state_nodes$attribute,' ','_')

write_csv(state_nodes,'state_nodes.csv')

#description
write_csv(create_description_csv(description_of_article = 'Study the compartmentalization in plants-insects webs in two sites.', source_of_data = 'Web_of_Life', data_url = references$data_url),'description.csv')

