library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
reference <- tibble(doi=NA, author='G. Kohler',year='15/06/2011',paper_url='http://hdl.handle.net/1884/25658',data_url= c('http://www.web-of-life.es/map.php?type=6','http://www.ecologia.ib.usp.br/iwdb/html/schleuning-et-al-2010.html'))
write_csv(reference, 'references.csv')

#layers
ref <- read_csv("web_reference.csv")
elevation <- c('350-600 m','600-850 m','850-1100 m')
layers <- NULL
for (i in 1:nrow(ref)) {
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=ref$ID[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value='environment'))
  layers <- rbind(layers,tibble(layer=i,attribute='species',value=ref$Species[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='location',value=ref$`Locality of Studi`[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='elevation',value=elevation[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='interactions',value=ref$Interactions[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='connectance',value=ref$Connectance[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value=ref$Latitude[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value=ref$Longitude[i]))

}

write_csv(layers,'layers.csv')

#interactions
inter_table <- function (file_name, layer) {
  countryx <- read_csv(file_name)
  countryx <- countryx[-1,]
  country_x_names <- countryx[['...1']]
  countryx <- countryx[,-1]
  rownames(countryx) <- country_x_names
  view(countryx)
  interaction_yearx <- matrix_to_list_bipartite(countryx,group_names = c('Pollinator','Plants'))$edge_list
  interaction_yearx <- tibble(node_from=interaction_yearx$from, layer_from= layer, node_to=interaction_yearx$to,
                              layer_to = layer, weight=interaction_yearx$weight,type='pollination')
}

inter_1 <- inter_table('M_PL_069_01.csv', 1)
inter_2 <-  inter_table('M_PL_069_02.csv', 2)
inter_3 <-  inter_table('M_PL_069_03.csv', 3)

interactions_test <- rbind(inter_1,inter_2,inter_3)

interactions <- NULL
for (i in 1:nrow(interactions_test)){
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='node_from',value=interactions_test$node_from[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='layer_from',value=interactions_test$layer_from[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='node_to',value=interactions_test$node_to[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='layer_to',value=interactions_test$layer_to[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='weight',value=interactions_test$weight[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='type',value=rep('pollination')))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='method',value='observation'))

}


#check there is no repetitions in the interaction id
test <- unique(interactions$interaction_id)

write_csv(interactions,'interactions.csv')

#nodes
M_PL_069_01_species <- read_csv("M_PL_069_01-species.csv")
M_PL_069_02_species <- read_csv("M_PL_069_02-species.csv")
M_PL_069_03_species <- read_csv("M_PL_069_03-species.csv")

all_nodes <- unique(rbind(M_PL_069_01_species[,1:3],M_PL_069_02_species[,1:3],M_PL_069_03_species[,1:3]))
all_nodes$Specie <- str_replace_all(all_nodes$Specie,' ','_')

nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=str_replace_all(all_nodes$Specie[i],' ','_')))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= all_nodes$Role[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='kingdom',value= all_nodes$Kingdom[i]))
}

write_csv(nodes,'nodes.csv')

#state nodes
M_PL_069_01_species <- add_column(M_PL_069_01_species,layer_id = 1,.before = 'Kingdom',node_id = NA)
M_PL_069_02_species <- add_column(M_PL_069_02_species,layer_id = 2,.before = 'Kingdom',node_id = NA)
M_PL_069_03_species <- add_column(M_PL_069_03_species,layer_id = 3,.before = 'Kingdom',node_id = NA)
M_PL_069_01_species$Specie<-str_replace_all(M_PL_069_01_species$Specie,' ','_')
M_PL_069_02_species$Specie<-str_replace_all(M_PL_069_02_species$Specie,' ','_')
M_PL_069_03_species$Specie<-str_replace_all(M_PL_069_03_species$Specie,' ','_')

state_nodes <- rbind(M_PL_069_01_species,M_PL_069_02_species,M_PL_069_03_species)

state_nodes <- state_nodes_long(state_nodes)

write_csv(state_nodes,'state_nodes.csv')

#description
write_csv(create_description_csv(description_of_article = 'Data describing the interactions between hummingbirds and floral resources in an altitudinal gradient in the Brazilian Atlantic Rainforest.', source_of_data = c('Web_of_Life','Interaction_Web_DataBase'),data_url = reference$data_url),'description.csv')

