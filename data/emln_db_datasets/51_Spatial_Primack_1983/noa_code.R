library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
references <- tibble(doi='10.1080/0028825X.1983.10428561',author='Richard B. Primack',year='27/01/1983', paper_url = 'https://doi.org/10.1080/0028825X.1983.10428561',
                    data_url = 'http://www.web-of-life.es')

write_csv(references, 'references.csv')

#nodes
M_PL_027_species <- read_csv("M_PL_027-species.csv")
M_PL_028_species <- read_csv("M_PL_028-species.csv")
M_PL_029_species <- read_csv("M_PL_029-species.csv")
all_nodes <- rbind(M_PL_027_species[,-4:-6],M_PL_028_species[,-4:-6],M_PL_029_species[,-4:-6])
all_nodes <- unique(all_nodes)

all_nodes$Specie <- all_nodes$Specie %>% str_replace_all(' ','_')

nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes$Specie[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= all_nodes$Role[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='kingdom',value=all_nodes$Kingdom[i]))

}


write_csv(nodes,'nodes.csv')

#state nodes
state_nodes <- c()

temp_all <- cbind(tibble(layer_id = c(rep.int(1,times = nrow(M_PL_027_species)), rep.int(2,times = nrow(M_PL_028_species)), rep.int(3, nrow(M_PL_029_species)))), rbind(M_PL_027_species,M_PL_028_species,M_PL_029_species))
names <- c(M_PL_027_species$Specie,M_PL_028_species$Specie,M_PL_029_species$Specie)
for (i in 1:nrow(temp_all)) {
  print(i)
  node_id_temp <- filter(nodes, attribute == 'node_name', value == str_replace_all(names[[i]],' ','_'))$node_id #the physical node id for the state node
  print(node_id_temp)
  node_name <- filter(nodes, node_id == node_id_temp, attribute == 'node_name')$value
  print(node_name)
  state_nodes <- rbind(state_nodes, tibble(layer_id = rep(temp_all$layer_id[i]), node_id = node_id_temp, attribute = c('node_name','degree','connections_strength','presence'),
                                           value = c(node_name,temp_all[i,]$Degree,temp_all[i,]$`Sum(connections strength)`,temp_all[i,]$`Networks presence`)))
}


write_csv(state_nodes, 'state_nodes.csv')




#layers
references_M_PL_027 <- read_csv("references_M_PL_027.csv")
references_M_PL_028 <- read_csv("references_M_PL_028.csv")
references_M_PL_029 <- read_csv("references_M_PL_029.csv")

ref <- rbind(references_M_PL_027,references_M_PL_028,references_M_PL_029)
ref$`Type of interactions` <- tolower(ref$`Type of interactions`)
layers <- NULL
for (i in 1:length(ref$ID)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=ref$ID[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers <- rbind(layers,tibble(layer=i,attribute='species',value=ref$Species[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='location',value=ref$`Locality of Studi`[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='interactions',value=ref$Interactions[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='connectance',value=ref$Connectance[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value=ref$Latitude[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value=ref$Longitude[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type_of_interactions',value=ref$`Type of interactions`[i]))
  

}

write_csv(layers,'layers.csv')

#interactions
inter_table <- function (file_name, layer) {
  countryx <- read_csv(file_name)
  country_x_names <- countryx[['...1']]
  countryx <- countryx[,-1]
  rownames(countryx) <- country_x_names
  rownames(countryx) <- str_replace_all(rownames(countryx),' ','_')
  colnames(countryx) <- str_replace_all(colnames(countryx),' ','_')
  interaction_yearx <- matrix_to_list_bipartite(countryx,group_names = c('Pollinators','Plants'))$edge_list
  interactions <- tibble(node_from = interaction_yearx$from, layer_from = layer, node_to= interaction_yearx$to, layer_to = layer, weight = interaction_yearx$weight, 
                         method = 'observation')
  return(interactions)
}

inter_1 <- inter_table('M_PL_027.csv','M_PL_027')
inter_2 <- inter_table('M_PL_028.csv','M_PL_028')
inter_3 <- inter_table('M_PL_029.csv','M_PL_029')

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

#check 
test <- unique(interactions$interaction_id)

write_csv(interactions,'interactions.csv')

#description
write_csv(create_description_csv(description_of_article = 'specialisation of plants and insects', source_of_data = 'Web_of_Life',data_url = references$data_url),'description.csv')



