library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()


#reference
references <- tibble(doi='10.2307/3544990',author='Simon R. Leather',year='01.01.1997', paper_url = 'https://www.jstor.org/stable/3544990', data_url = c('http://www.web-of-life.es/map.php','http://www.ecologia.ib.usp.br/iwdb/html/leather_1991.htm'))
write_csv(references, 'references.csv')

#nodes
A_PH_006_species <- read_csv("A_PH_006-species.csv")
A_PH_007_species <- read_csv("A_PH_007-species.csv")

all_nodes <- rbind(A_PH_006_species,A_PH_007_species)[,-4:-6]
all_nodes <- unique(all_nodes)

# test <- tibble(node_id=1:length(all_nodes$Specie), node_name=all_nodes$Specie, type=all_nodes$Role,
#                 attribute1 = 'kingdom', value1=all_nodes$Kingdom,
#                 attribute2= 'degree', value2=all_nodes$Degree,
#                 attribute3='connections strength', value3=all_nodes$`Sum(connections strength)`,
#                 attribute4='networks presence',value4=all_nodes$`Networks presence`)


nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=str_replace_all(all_nodes$Specie[i],' ','_')))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= all_nodes$Role[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='kingdom',value=all_nodes$Kingdom[i]))
  # nodes <- rbind(nodes,tibble(node_id=i,attribute='connections_strength',value=all_nodes$`Sum(connections strength)`[i]))
  # nodes <- rbind(nodes,tibble(node_id=i,attribute='network_presence',value=all_nodes$`Networks presence`[i]))
  # nodes <- rbind(nodes,tibble(node_id=i,attribute='connections_strength',value=all_nodes$Degree[i]))
  
}


write_csv(nodes,'nodes.csv')


#state nodes
state_nodes <- c()
temp_all <- list()

temp_all <- cbind(tibble(layer_id = c(rep.int(1,times = 94), rep.int(2,times = 69))), rbind(A_PH_006_species,A_PH_007_species))
names <- c(A_PH_006_species$Specie,A_PH_007_species$Specie)
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
ref <- read_csv("references231231.csv")

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

}

write_csv(layers,'layers.csv')

#interactions
inter_table <- function (file_name, layer) {
  countryx <- read_csv(file_name)
  country_x_names <- countryx[['...1']]
  countryx <- countryx[,-1]
  rownames(countryx) <- country_x_names
  interaction_yearx <- matrix_to_list_bipartite(countryx,group_names = c('Herbivore','Plants'))$edge_list
  interactions <- tibble(node_from = interaction_yearx$from, layer_from = layer, node_to= interaction_yearx$to, layer_to = layer, weight = interaction_yearx$weight, 
                         method = 'observation')
  return(interactions)
}

A_PH_006 <- inter_table('A_PH_006.csv','A_PH_006')
A_PH_007 <- inter_table('A_PH_007.csv','A_PH_007')

interactions_test <- rbind(A_PH_006,A_PH_007)

interactions <- NULL
for (i in 1:nrow(interactions_test)){
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='node_from',value=interactions_test$node_from[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='layer_from',value=interactions_test$layer_from[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='node_to',value=interactions_test$node_to[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='layer_to',value=interactions_test$layer_to[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='weight',value=interactions_test$weight[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='type',value=rep('herbivory')))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='method',value='observation'))
  
}

#check
test<- unique(interactions$interaction_id)

write_csv(interactions,'interactions.csv')

#description
write_csv(create_description_csv(description_of_article = 'feeding specialisation and host distribution', source_of_data = c('Web_of_Life','Interaction_Web_DataBase'),data_url = references$data_url), 'description.csv')





