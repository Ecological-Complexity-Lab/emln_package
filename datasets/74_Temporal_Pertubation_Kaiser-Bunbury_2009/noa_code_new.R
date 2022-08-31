library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
reference1 <- tibble(doi='10.1111/j.1461-0248.2009.01437.x',author='Christopher N. Kaiser-Bunbury',year='15/03/2010', paper_url = 'https://onlinelibrary.wiley.com/doi/10.1111/j.1461-0248.2009.01437.x',
                     data_url = c('http://www.ecologia.ib.usp.br/iwdb/html/kaiser-bunbury_2009.html','http://www.web-of-life.es/map.php'))
reference2 <- tibble(doi='10.1016/j.ppees.2009.04.001',author='Christopher N. Kaiser-Bunbury',year='20/11/2009', paper_url = 'https://www.sciencedirect.com/science/article/pii/S1433831909000183?via%3Dihub',
                     data_url = c('http://www.ecologia.ib.usp.br/iwdb/html/kaiser-bunbury_2009.html','http://www.web-of-life.es/map.php'))

references <- rbind(reference1,reference2)

write_csv(references, 'references.csv')

#layers
layer_info <- read_excel("Mauritius 03_KaiserBunbury.xls",
                         sheet = "matrix", range = "A1:C806")
layer_info <- distinct(layer_info, .keep_all = T)
sites <- layer_info$NetworkID %>% unique()

pertrubed <- c()
for (i in 1:nrow(layer_info)) {
  if (layer_info$Site[i] == 'CMA') {
    pertrubed <- append(x = pertrubed, values = F)
  }
  else {
    pertrubed <- append(x = pertrubed, values = T)
  }
}
layers <- NULL
for (i in 1:length(sites)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=sites[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value=c('time','perturbation')))
  layers <- rbind(layers,tibble(layer=i,attribute='perturbed',value=pertrubed[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='site',value=str_replace_all(sites[i],'\\d','')))
  layers <- rbind(layers,tibble(layer=i,attribute='period',value=layer_info$Period[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='location', value ='Black River Gorges National Park, Mauritius; Collected at Le Pétrin CMA and an adjacent unrestored (nonmanaged) site (Control)'))
  layers <- rbind(layers,tibble(layer=i,attribute='perturbation_type',value='invasion of exotic plants'))
  layers <- rbind(layers,tibble(layer=i,attribute='sample_dates',value='Between 15th September 2003 and 15th March 2004, 2-weeks networks'))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value=-20.7))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value=57.733333))
}

write_csv(layers,'layers.csv')


#nodes
nodes <- read_excel("Mauritius 03_KaiserBunbury.xls",
                         sheet = "species")
nodes$node_id <- 1:nrow(nodes)
nodes$type <- NA
colnames(nodes)[4] <- 'node_name'
colnames(nodes) %<>% tolower() %>%
  str_replace_all(' ','_')

for (i in 1:nrow(nodes)) {
  if (nodes[i,]$kingdom == 'Animal') {
    nodes$type[i] <- 'pollinator'
  } else {
    nodes$type[i] <- 'plant'
  }
}

nodes$node_name %<>% str_replace_all(' ','_')

nodes <- nodes %>%
  select(node_id, node_name, type, everything())

nodes_long <- nodes %>%
  gather(key=attribute, value, -node_id) %>%
  arrange(node_id)

write_csv(nodes_long, 'nodes.csv')



#interactions
interaction_agg <- read_excel("Mauritius 03_KaiserBunbury.xls",
                    sheet = "matrix")
interaction1 <- interaction_agg[interaction_agg$NetworkID == sites[i],][,-1:-3]
interaction1 <- column_to_rownames(interaction1, '...4')
interaction1[is.na(interaction1)] <- 0

interactions <- NULL
for (i in 1:24) {
  interaction1 <- interaction_agg[interaction_agg$NetworkID == sites[i],][,-1:-3]
  interaction1 <- column_to_rownames(interaction1, '...4')
  interaction1 <- interaction1[-nrow(interaction1),]
  interaction1[is.na(interaction1)] <- 0
  inter <- matrix_to_list_bipartite(interaction1,group_names = c('pollinators','plants'))$edge_list
  interactions <- rbind(interactions,tibble(node_from = inter$from, layer_from = layers[layers$value == sites[i],]$layer, node_to = inter$to,
                         layer_to = layers[layers$value == sites[i],]$layer, weight = inter$weight,type ='pollination', method = 'observation'))
}

interactions$node_from <- str_replace_all(interactions$node_from, ' ','_')
interactions<-add_column(interaction_id = 1:nrow(interactions),interactions)

interactions_long <- interactions %>%
  gather(key=attribute, value, -interaction_id) %>%
  arrange(interaction_id)

write_csv(interactions_long, 'interactions.csv')

#description
write_csv(create_description_csv(description_of_article = 'This paper describes the community structure of pollination webs of Mauritian heathland habitats. Visitation rates are calculated and  compared between areas that are heavily invaded by introduced plant species (control site) and areas from which non-native plants were removed (restored site).', source_of_data = c('Interaction_Web_DataBase','Web_of_Life'),data_url = references$data_url),'description.csv')

