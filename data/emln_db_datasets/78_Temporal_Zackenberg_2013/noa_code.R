library(tidyverse)
library(bipartite)
library(rstudioapi)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()


#layers
d <- read_csv('ZAC CR.csv') 
layer_names <- c(unique(d$Date_collected))
view(layer_names)

layers <- NULL
for (i in 1:length(layer_names)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=(layer_names[i])))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value='time'))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value=74.466667))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value=-20.583333))
  layers <- rbind(layers,tibble(layer=i,attribute='location',value= 'high–arctic research station Zackenberg, NE Greenland'))
  
  
}

write_csv(layers, 'layers.csv')

#nodes
plants <- unique(d$Plant)
pollinators <- unique(d$Pollinator)

nodes <- NULL
for (i in 1:length(c(plants,pollinators))){
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=(c(plants,pollinators)[i])))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value=c(rep.int('plant',length(plants)),rep.int('pollinator',length(pollinators)))[i]))
  
}

write_csv(nodes, 'nodes.csv')

#interactions
test <- tibble(node_from=d$Pollinator, layer_from=d$Date_collected, node_to=d$Plant,
                       layer_to=d$Date_collected, weight=1, type='pollination',
                       attribute1 = 'method', value1= 'observation')

interactions <- NULL
for (i in 1:nrow(test)){
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='node_from',value=d$Pollinator[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='layer_from',value=d$Date_collected[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='node_to',value=d$Plant[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='layer_to',value=d$Date_collected[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='weight',value=rep(1)))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='type',value=rep('pollination')))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='method',value='observation'))
  
}

d <- d %>% 
  add_column(interaction_id = 1:nrow(d), .before = 1) %>%
  add_column(type = 'pollination') %>%
  add_column(method = 'observation') %>%
  add_column('layer_to' = d$Date_collected, .before = 4) %>% 
  rename('layer_from' = Date_collected) %>%
  rename('node_from' = Pollinator) %>%
  rename('node_to' = Plant) 
  

interactions <- d %>% melt(id.vars = 'interaction_id') %>%
  arrange(interaction_id) %>%
  rename('attribute' = variable)

write_csv(interactions, 'interactions.csv')

#references
references <- tibble(doi='10.1371/journal.pone.0081694',author='Claus Rasmussen', year='04/12/2013',paper_url='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0081694', data_url='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0081694')
write_csv(references, 'references.csv')

#description
write_csv(create_description_csv(description_of_article = 'the impact of temporal resolution on the structure of an plant-pollinator ecological network.',source_of_data = 'plos',data_url = references$data_url), 'description.csv')
  
nodes[nodes$attribute == 'node_name',]$value
length(unique(interactions[interactions$attribute == 'node_from' | interactions$attribute == 'node_to',]$value))

length(unique(c(d$Pollinator,d$Plant)))
