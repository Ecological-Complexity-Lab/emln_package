library(tidyverse)
library(bipartite)
library("rmangal")

datasets<-search_datasets(list(ref_id = 155))
datasets<-get_collection(datasets)

#layers
layers=NULL
for(i in 1:8){
  if(i!=1){
    layers<-rbind(layers,tibble(layer=i-1,attribute='type',value='time'))
    date<-datasets[[i]]$network$date
    layers<-rbind(layers,tibble(layer=i-1,attribute='date',value=paste0(substr(date,9,10),'/',substr(date,6,7),'/',substr(date,1,4))))
    layers<-rbind(layers,tibble(layer=i-1,attribute='description',value="Food web structure of the macroinvertebrate riffle community"))
    layers<-rbind(layers,tibble(layer=i-1,attribute='location',value="Duffin Creek, Ontario, Canada"))
    layers<-rbind(layers,tibble(layer=i-1,attribute='latitude',value=datasets[[i]]$network$geom_lat[[1]]))
    layers<-rbind(layers,tibble(layer=i-1,attribute='longitude',value=datasets[[i]]$network$geom_lon[[1]]))
    layers<-rbind(layers,tibble(layer=i-1,attribute='directed',value=TRUE))
  }
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_Tavares_Cromar1996/layers.csv')

#interactions & nodes
interactions<-NULL
nodes<-NULL
for(i in 1:8){
  if(i!=1){
    interactions<-rbind(interactions,tibble(node_from=datasets[[i]]$interactions$node_from,layer_from=i-1,
                                            node_to=datasets[[i]]$interactions$node_to,layer_to=i-1,
                                            weight=datasets[[i]]$interactions$value,type=datasets[[i]]$interactions$type,
                                            attribute1="method",value1=datasets[[i]]$interactions$method))
    
    nodes<-rbind(nodes,tibble(node_id=datasets[[i]]$nodes$node_id,node_name=datasets[[i]]$nodes$original_name,
                              type=datasets[[i]]$nodes$node_level,
                              attribute1="taxonomy_name",value1=datasets[[i]]$nodes$taxonomy.name,
                              attribute2="taxonomy_rank",value2=datasets[[i]]$nodes$taxonomy.rank))
  }
}
v<-interactions$type=="unspecified"
interactions$type[v]<-"trophic"
for(i in 1:nrow(interactions)){
  interactions$node_from[i]<-filter(nodes,node_id==interactions$node_from[i])$node_name
  interactions$node_to[i]<-filter(nodes,node_id==interactions$node_to[i])$node_name
}
interactions$node_to <- str_replace_all(interactions$node_to, 'Fungal Hyphae', 'Fungal hyphae')

write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_Tavares_Cromar1996/interactions.csv')

nodes$node_name <- str_replace_all(nodes$node_name, 'Fungal Hyphae', 'Fungal hyphae')
nodes<-nodes %>% distinct(node_name, .keep_all=TRUE)
nodes$node_id<-1:nrow(nodes)
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_Tavares_Cromar1996/nodes.csv')

#references
references<-tibble(doi=datasets[[1]]$reference$doi,author=datasets[[1]]$reference$first_author,
                   year=datasets[[1]]$reference$year,paper_url=datasets[[1]]$reference$paper_url)
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_Tavares_Cromar1996/references.csv')

