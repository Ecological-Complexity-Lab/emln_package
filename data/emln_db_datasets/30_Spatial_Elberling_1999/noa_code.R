library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
references <- tibble(doi=NA,author='Heidi Elberling & Jens M. Olesen',year='09/09/1998',paper_url='https://www.jstor.org/stable/3683041',data_url=c('http://www.web-of-life.es/map.php?type=6','http://www.ecologia.ib.usp.br/iwdb/'))
references <- rbind(references, tibble(doi=NA, author= 'Heidi Elberling & Jens M. Olesen', year= 'unpublished' ,paper_url=NA, data_url=c('http://www.web-of-life.es/map.php?type=6','http://www.ecologia.ib.usp.br/iwdb/')))

write_csv(references, 'references.csv')

#layers
ref <- read_csv("web_references.csv")

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
  # country_x_names <- countryx[['...1']]
  # countryx <- countryx[,-1]
  # rownames(countryx) <- country_x_names
  countryx<-column_to_rownames(countryx,var = 'X1')
  view(countryx)
  interaction_yearx <- matrix_to_list_bipartite(countryx,group_names = c('Pollinator','Plants'))$edge_list
  interaction_yearx <- tibble(node_from=interaction_yearx$from, layer_from= layer, node_to=interaction_yearx$to, 
                              layer_to = layer, weight=interaction_yearx$weight,type='pollination', method = 'observation')}

inter_1 <- inter_table('M_PL_009.csv', '1')
inter_2 <-  inter_table('M_PL_010.csv', '2')

interactions_test <- rbind(inter_1,inter_2)

interactions <- inter_long(interactions_test)

#check
test <- unique(interactions$interaction_id)

write_csv(interactions,'interactions.csv')

#nodes
M_PL_009_species <- read_csv("M_PL_009-species.csv")
M_PL_010_species <- read_csv("M_PL_010-species.csv")

all_nodes <- rbind(M_PL_009_species,M_PL_010_species)
all_nodes <- all_nodes[,-4:-6] %>% unique()
species_names <- unique(all_nodes$Specie)

all_nodes$Specie <- str_replace_all(all_nodes$Specie, ' ','_')
all_nodes$Role <- tolower(all_nodes$Role)
all_nodes$Kingdom <- tolower(all_nodes$Kingdom)

nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes$Specie[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= all_nodes$Role[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='kingdom',value=all_nodes$Kingdom[i]))
}

write_csv(nodes,'nodes.csv')


#state nodes 
state_nodes <- rbind(M_PL_009_species,M_PL_010_species)
state_nodes<-add_column(state_nodes,layer_id = c(rep.int(1,142),rep.int(2,107)), node_id = NA)

state_nodes<-state_nodes_long(state_nodes)

write_csv(state_nodes,'state_nodes.csv')
#description
write_csv(create_description_csv(description_of_article = ' The objective was to describe the plant-flower visitor interaction matrix of this area and compare it with the characteristics of other subarctic alpine systems and with pollination systems of lower latitudes, especially in relation to the role of flies as flower visitors at high latitudes.', source_of_data = c('Web_of_Life','Interaction_Web_DataBase'),data_url = references$data_url),'description.csv')
