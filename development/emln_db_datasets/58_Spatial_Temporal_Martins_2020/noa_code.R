library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#references
write_csv(create_references_csv(doi = '10.1111/btp.12791',author = 'Lucas Pereira Martins',year = as.Date(parse_date_time('13/04/2020',"dmy")),paper_url = 'https://onlinelibrary.wiley.com/doi/full/10.1111/btp.12791'
                                ,data_url = 'https://doi.org/10.5061/dryad.vmcvdncq0'),'references.csv')


#layers
layer_names_files <- list.files(pattern = 'site\\d+_period\\d+',full.names = FALSE)
layer_names_files <- str_remove_all(layer_names_files,'.csv')

layers <- NULL
for (i in 1:length(layer_names_files)){
  name <- layer_names_files[i]
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=name))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value=c('space','time')))
  layers <- rbind(layers,tibble(layer=i,attribute='location',value='remnants of Cerrado vegetattion in Sao Paulo, Brazil'))
  layers <- rbind(layers,tibble(layer=i,attribute='mean_distance_between_sites',value='11.3 km'))
  if (str_sub(name,start = -1,end = -1) == 1) {
    layers <- rbind(layers,tibble(layer=i,attribute='period',value='april-may 2004'))
    layers <- rbind(layers,tibble(layer=i,attribute='temporal_herbivore_abundance',value=77))
    layers <- rbind(layers,tibble(layer=i,attribute='temporal_plant_abundance',value=29))
  } else if (str_sub(name,start = -1,end = -1) == 2) {
    layers <- rbind(layers,tibble(layer=i,attribute='period',value='august-september 2004'))
    layers <- rbind(layers,tibble(layer=i,attribute='temporal_herbivore_abundance',value=26))
    layers <- rbind(layers,tibble(layer=i,attribute='temporal_plant_abundance',value=12))
  } else {
    layers <- rbind(layers,tibble(layer=i,attribute='period',value='january-feburary 2004'))
    layers <- rbind(layers,tibble(layer=i,attribute='temporal_herbivore_abundance',value=42))
    layers <- rbind(layers,tibble(layer=i,attribute='temporal_plant_abundance',value=22))
  }
}

write_csv(layers,'layers.csv')

#interactions
site_interactions_files <- list.files(pattern = 'site\\d+_period\\d+.csv',full.names = TRUE)


interactions <- NULL
for (i in 1:length(site_interactions_files)) {
  x<-read_csv(site_interactions_files[i])
  row_names <- x[[1]]
  x %<>% select(-1) %>%
    as.data.frame()
  rownames(x) <- row_names
  edge_list <- matrix_to_list_bipartite(x,c('herbivore','plant'))$edge_list
  colnames(edge_list) <- c('node_from','node_to','weight')
  edge_list %<>%
    add_column('layer_from' = i,.before = 'node_to') %>%
    add_column('layer_to' = i, .before = 'weight') %>%
    add_column('type' = 'herbivory') %>%
    add_column('method' = 'field_study')
  interactions <- rbind(interactions,edge_list)
}
interactions <- cbind(interaction_id = 1:nrow(interactions),interactions)
interactions <- pivot_longer(interactions, !interaction_id, names_to = 'attribute',values_to = 'value', values_transform = as.character)


write_csv(inter_long_att(interactions),'interactions.csv')

#nodes
plants<-read_excel('List of plant species .xlsx')
plants <- add_column(type = 'plant',plants, .before = 2)
herbivores<-read_excel('List of herbivore species.xlsx')
herbivores <- add_column(type = 'herbivore',herbivores, .before = 2)

all_nodes <- rbind(plants[,1:2],herbivores[,1:2])

nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes$...1[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= all_nodes$type[i]))
}
nodes <- nodes %>%
  mutate(across(where(is.character), str_replace_all, ' ', '_'))

write_csv(nodes,'nodes.csv')

#state nodes
state_names <- str_replace_all(c(plants$...1,herbivores$...1),' ','_')
presence <- rbind(plants,herbivores)[,-1:-2]
state_nodes <- NULL
layer_sites <- unlist(c(list(1:3),list(4:6)))
#filter(nodes, attribute == 'node_name' & value == state_names[i])$node_id
for (i in 1:(nrow(plants)+nrow(herbivores))) {
  for (j in 1:10) {
  state_nodes <- rbind(state_nodes,tibble(layer_id = filter(layers, str_detect(value,
                                           pattern = ifelse(j == 10, paste0('site',j,'_*'),
                                           paste0('site0',j,'_*'))))$layer,
                                           node_id = i,
                                          attribute = 'node_name',
                                          value = state_names[i]))
  state_nodes <- rbind(state_nodes, tibble(layer_id = filter(layers, str_detect(value,
                                                                                pattern = ifelse(j == 10, paste0('site',j,'_*'),paste0('site0',j,'_*'))))$layer,
                                           node_id = i,
                                           attribute = 'presence',
                                           value = unlist(presence[i,j])
                                           ))
  }
}

state_nodes %<>% group_by(layer_id, node_id) %>%
  arrange(layer_id, node_id)

write_csv(state_nodes, 'state_nodes.csv')

#description
write_csv(create_description_csv(description_of_article = 'Here, we used a spatial-temporal system comprising asteraceous plants and flower head herbivores from remnants of Brazilian Cerrado to investigate whether the relationship between spatial beta-diversity of species and network structural dissimilarity changes over time.',source_of_data = 'Dryad',data_url = 'https://doi.org/10.5061/dryad.vmcvdncq0'),'description.csv')



