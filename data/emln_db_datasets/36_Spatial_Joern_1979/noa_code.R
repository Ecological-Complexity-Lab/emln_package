library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
references <- tibble(doi='10.1007/BF00345192',author='Anthony Joern',year='23/10/1978',
                    paper_url ='https://doi.org/10.1007/BF00345192',
                    data_url = c('http://www.web-of-life.es/map.php?type=6','http://www.ecologia.ib.usp.br/iwdb/html/joern_1979.htm'))
write_csv(references, 'references.csv')

#nodes
A_PH_004_species <- read_csv("A_PH_004-species.csv")
A_PH_005_species <- read_csv("A_PH_005-species.csv")


all_nodes <- unique(rbind(A_PH_004_species,A_PH_005_species)[,-4:-6])

all_nodes$Specie <- str_replace_all(all_nodes$Specie,' ','_')
all_nodes$Role <- tolower(all_nodes$Role)
all_nodes$Kingdom <- tolower(all_nodes$Kingdom)

nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes$Specie[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= all_nodes$Role[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='kingdom',value=all_nodes$Kingdom[i]))
}
nodes[nodes$attribute=='node_name',]$value <- gsub(nodes[nodes$attribute=='node_name',]$value,pattern = '_A_PH_(004|005)',replacement = '')

nodes <- nodes_wide %>% pivot_longer(!node_id, names_to = 'attribute', values_to = 'value', values_transform = list(value=as.character))

write_csv(nodes,'nodes.csv')


#state nodes
state_nodes <- c()
temp_all <- list()
temp_all[['A_PH_004']] <-  A_PH_004_species
temp_all[['A_PH_005']] <- A_PH_005_species

temp_all <- cbind(tibble(layer_id = c(rep.int(1,times = 74), rep.int(2,times = 78))), rbind(A_PH_004_species,A_PH_005_species))
temp_all$Specie <- gsub(temp_all$Specie,pattern = '_A_PH_(004|005)',replacement = '')
temp_all$Specie <- gsub(temp_all$Specie,pattern = ' ',replacement = '_')
nodes_wide <-  nodes %>%
  pivot_wider(id_cols = node_id, names_from = 'attribute',values_from = 'value')
nodes_wide <- nodes_wide[,-1]
nodes_wide <- distinct(nodes_wide)
nodes_wide <- cbind(node_id=1:nrow(nodes_wide),nodes_wide)
state_nodes <- NULL
for (i in 1:nrow(nodes_wide)) {
  if(i==44) {print(i)}
    node_name <- filter(nodes_wide, node_id == i)$node_name
    layer_i <- filter(temp_all, Specie==node_name)$layer_id
    state_nodes <- rbind(state_nodes, tibble(layer_id = layer_i, node_id = rep(i),
                                             node_name = node_name,
                                             degree=temp_all[temp_all$Specie%in%node_name & temp_all$layer_id%in%layer_i,]$Degree,
                                             connections_strength=temp_all[temp_all$Specie%in%node_name & temp_all$layer_id%in%layer_i,]$`Sum(connections strength)`,
                                             presence=temp_all[temp_all$Specie%in%node_name & temp_all$layer_id%in%layer_i,]$`Networks presence`))
}

state_nodes <- state_nodes %>%
  group_by(layer_id,node_id) %>%
  pivot_longer(!c(layer_id,node_id), names_to = 'attribute', values_to = 'value', values_transform = list(value=as.character)) %>%
  arrange()

write_csv(state_nodes, 'state_nodes.csv')


#layers
references1 <- read_csv("web_references.csv")
references2 <- read_csv("web_references copy.csv")
ref <- rbind(references2,references1)

layer_names <- ref$ID

layers <- NULL
for (i in 1:length(layer_names)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=(layer_names[i])))
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
  interaction_yearx <- tibble(node_from=interaction_yearx$from, layer_from= layer, node_to=interaction_yearx$to,
                              layer_to = layer, weight=interaction_yearx$weight,type='herbivory', method = 'observation')
  return(interaction_yearx)
}

A_PH_004 <- inter_table("A_PH_004.csv",'A_PH_004')
A_PH_005 <- inter_table("A_PH_005.csv",'A_PH_005')

interactions_test <- rbind(A_PH_004,A_PH_005)
interactions_test$node_from <- str_replace_all(interactions_test$node_from,' ','_')
interactions_test$node_to <- str_replace_all(interactions_test$node_to,' ','_')

#turning into long format
interactions <- inter_long(interactions_test)

#check
test <- unique(interactions$interaction_id)


interactions[interactions$attribute%in%c('node_from','node_to'),]$value <- gsub(interactions[interactions$attribute%in%c('node_from','node_to'),]$value,pattern = '_A_PH_(004|005)',replacement = '')
interactions[interactions$attribute%in%c('node_from','node_to'),]$value <- gsub(interactions[interactions$attribute%in%c('node_from','node_to'),]$value,pattern = ' ',replacement = '_')


write_csv(interactions,'interactions.csv')

#description
write_csv(create_description_csv(description_of_article = 'In this paper, the authors examined the feeding patterns of grasshoppers from two arid grassland communities in Trans-Pecos, Texas. ', source_of_data = c('Web_of_Life','Interaction_Web_DataBase'),data_url = references$data_url),'description.csv')

