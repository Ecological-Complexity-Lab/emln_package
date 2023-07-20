library(tidyverse)
library(bipartite)
library("rmangal")

datasets<-search_datasets(list(ref_id = 81))
datasets<-get_collection(datasets)
layers=NULL
locations<-c("Lake_La_Muerte,Los Monegros,Spain","Lake_Pinol,Los Monegros,Spain","Lake_Pinol,Los Monegros,Spain","Lake_La_Muerte,Los Monegros,Spain")
for(i in 1:4){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space & time'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=locations[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=datasets[[i]]$network$geom_lat[[1]]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=datasets[[i]]$network$geom_lon[[1]]))
  date<-datasets[[i]]$network$date
  layers<-rbind(layers,tibble(layer=i,attribute='date',value=paste0(substr(date,9,10),'/',substr(date,6,7),'/',substr(date,1,4))))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Temporal_Alcorlo2001/layers.csv')

#interactions & nodes
interactions<-NULL
nodes<-NULL
for(i in 1:4){
  interactions<-rbind(interactions,tibble(node_from=datasets[[i]]$interactions$node_from,layer_from=i,
                                          node_to=datasets[[i]]$interactions$node_to,layer_to=i,
                                          weight=1,type=datasets[[i]]$interactions$type,
                                          attribute1="method",value1=datasets[[i]]$interactions$method))
  
  nodes<-rbind(nodes,tibble(node_id=datasets[[i]]$nodes$node_id,node_name=datasets[[i]]$nodes$original_name,
                            type=datasets[[i]]$nodes$node_level,
                            attribute1="taxonomy_name",value1=datasets[[i]]$nodes$taxonomy.name,
                            attribute2="taxonomy_rank",value2=datasets[[i]]$nodes$taxonomy.rank))
}
for(i in 1:nrow(interactions)){
  interactions$node_from[i]<-filter(nodes,node_id==interactions$node_from[i])$node_name
  interactions$node_to[i]<-filter(nodes,node_id==interactions$node_to[i])$node_name
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Temporal_Alcorlo2001/interactions.csv')

nodes<-nodes %>% distinct(node_name, .keep_all=TRUE)
nodes$node_id<-1:nrow(nodes)
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Temporal_Alcorlo2001/nodes.csv')

#references
references<-NULL
references<-rbind(references,tibble(doi=datasets[[1]]$reference$doi,author=datasets[[1]]$reference$first_author,
                                   year=datasets[[1]]$reference$year,paper_url=datasets[[1]]$reference$paper_url))
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Temporal_Alcorlo2001/references.csv')



