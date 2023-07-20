library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
references <- tibble(doi='10.2307/4083882',author='Barbara Snow & David Snow',year='01/04/1971', paper_url = 'https://www.jstor.org/stable/4083882', data_url = c('http://www.ecologia.ib.usp.br/iwdb/html/snow_snow_1971.htm','http://www.web-of-life.es/map.php?type=6'))
references <- rbind(references, tibble(doi=NA,author='Barbara Snow & David Snow',year='01/01/1988', paper_url = 'https://ebookcentral-proquest-com.ezproxy.bgu.ac.il/lib/bgu-ebooks/detail.action?docID=655440', data_url = 'http://www.web-of-life.es/map.php?type=6'))
references <- mutate(references, year = as.Date(parse_date_time(references$year,"dmy")))
write_csv(references, 'references.csv')

#nodes
M_SD_010_species <- read_csv("M_SD_010-species.csv")
M_SD_011_species <- read_csv("M_SD_011-species.csv")

all_nodes <- rbind(M_SD_010_species[,-4:-6],M_SD_011_species[,-4:-6])
all_nodes <- unique(all_nodes)

all_nodes$Role <- tolower(all_nodes$Role)
all_nodes$Kingdom <- tolower(all_nodes$Kingdom)
all_nodes$Specie <- str_replace_all(all_nodes$Specie, ' ','_')
all_nodes$Role <- str_replace_all(all_nodes$Role,' ','_')


nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes$Specie[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= all_nodes$Role[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='kingdom',value=all_nodes$Kingdom[i]))
}


write_csv(nodes,'nodes.csv')


#state nodes
temp_all <- cbind(tibble(layer_id = c(rep.int(1,times = nrow(M_SD_010_species)), rep.int(2,times = nrow(M_SD_011_species))), rbind(M_SD_010_species,M_SD_011_species)))

state_nodes <- c()
names <- str_replace_all(temp_all$Specie,  ' ','_')
for (i in 1:nrow(temp_all)) {
  print(i)
  node_id_temp <- filter(nodes, attribute == 'node_name', value == names[[i]])$node_id #the physical node id for the state node
  print(node_id_temp)
  node_name <- filter(nodes, node_id == node_id_temp, attribute == 'node_name')$value
  print(node_name)
  state_nodes <- rbind(state_nodes, tibble(layer_id = rep(temp_all$layer_id[i]), node_id = node_id_temp, attribute = c('node_name','degree','connections_strength','presence'),
                                           value = c(node_name,temp_all[i,]$Degree,temp_all[i,]$`Sum(connections strength)`,temp_all[i,]$`Networks presence`)))
}

write_csv(state_nodes, 'state_nodes.csv')
#layers
referencesSD010 <- read_csv("referencesSD010.csv")
referencesSD011 <- read_csv("referencesSD011.csv")
ref <- rbind(referencesSD010,referencesSD011)

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
  rownames(countryx) <- str_replace_all(rownames(countryx),' ','_')
  colnames(countryx) <- str_replace_all(colnames(countryx),' ','_')
  interaction_yearx <- matrix_to_list_bipartite(countryx,group_names = c('Animals','Plants'))$edge_list
  interactions <- tibble(node_from = interaction_yearx$from, layer_from = layer, node_to= interaction_yearx$to, layer_to = layer, weight = interaction_yearx$weight, 
                         method = 'observation')
  return(interactions)
}

inter_1 <- inter_table('M_SD_010.csv','M_SD_010')
inter_2 <- inter_table('M_SD_011.csv','M_SD_011')

interactions_test <- rbind(inter_1,inter_2)

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
write_csv(create_description_csv(description_of_article = 'To what extent and in what ways the the feeding ecology differs among the various Trinidad bird species.',
                                 source_of_data = c('Web_of_Life','Interaction_Web_DataBase'),data_url = unique(references$data_url)), 'description.csv')


