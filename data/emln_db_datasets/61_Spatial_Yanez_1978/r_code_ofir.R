library(tidyverse)
library(bipartite)
library("rmangal")

datasets<-search_datasets(list(ref_id = 53))
datasets<-get_collection(datasets)

#layers
layers=NULL
for(i in 1:2){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='name',value=gsub(datasets[[i]]$network$description,pattern = ' ',replacement = '_')))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=datasets[[i]]$network$geom_lat[[1]]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=datasets[[i]]$network$geom_lon[[1]]))
  date<-datasets[[i]]$network$date
  layers<-rbind(layers,tibble(layer=i,attribute='date',value=paste0(substr(date,9,10),'/',substr(date,6,7),'/',substr(date,1,4))))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Yanez1978/layers.csv')

#interactions & nodes
interactions<-NULL
nodes<-NULL
for(i in 1:2){
  interactions<-rbind(interactions,tibble(node_from=datasets[[i]]$interactions$node_from,layer_from=gsub(x = layers[layers$layer == i & layers$attribute == 'name',]$value,pattern = ' ',replacement = '_'),
                                          node_to=datasets[[i]]$interactions$node_to,layer_to=gsub(x = layers[layers$layer == i & layers$attribute == 'name',]$value,pattern = ' ',replacement = '_'),
                                          weight=1,type="trophic"))

  nodes<-rbind(nodes,tibble(node_id=datasets[[i]]$nodes$node_id,node_name=datasets[[i]]$nodes$original_name,
                            type=datasets[[i]]$nodes$node_level,
                            attribute1="taxonomy_name",value1=datasets[[i]]$nodes$taxonomy.name,
                            attribute2="taxonomy_rank",value2=datasets[[i]]$nodes$taxonomy.rank))
}
for(i in 1:nrow(interactions)){
  interactions$node_from[i]<-gsub(x = filter(nodes,node_id==interactions$node_from[i])$node_name, pattern = ' ', replacement = '_')
  interactions$node_to[i]<-gsub(x = filter(nodes,node_id==interactions$node_to[i])$node_name, pattern = ' ', replacement = '_')
}


#revision!!! convert to long format
interactions$interaction_id <- 1:nrow(interactions)
interactions_long <- interactions %>% pivot_longer(!interaction_id, names_to = 'attribute',
                                     values_to = 'value',
                                     values_transform = list(value = as.character)) #convert to long format

write_csv(interactions_long, 'interactions.csv')

nodes<-nodes %>% distinct(node_name, .keep_all=TRUE)
nodes$node_id<-1:nrow(nodes)
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Yanez1978/nodes.csv')

#references
#references
references<-tibble(doi=NA,author="Yáñez-Arancibia, Alejandro",year="1978",
                   attribute1="title",value1="Taxonomía, ecología y estructura de las comunidades de peces en lagunas costeras con bocas efímeras del Pacífico de México")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Yanez1978/references.csv')
