library(tidyverse)
library(bipartite)
library("rmangal")

datasets<-search_datasets(list(ref_id = 50))
datasets<-get_collection(datasets)

#layers
layers=NULL
for(i in 1:2){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='interaction'))
  location<-datasets[[i]]$network$description
  if(i==1){
    layers<-rbind(layers,tibble(layer=i,attribute='name',value=substr(location,1,41)))
    layers<-rbind(layers,tibble(layer=i,attribute='location',value=substr(location,46,nchar(location))))
  }
  else{
    layers<-rbind(layers,tibble(layer=i,attribute='name',value=substr(location,1,39)))
    layers<-rbind(layers,tibble(layer=i,attribute='location',value=substr(location,44,nchar(location))))
  }
  date<-datasets[[i]]$network$date
  layers<-rbind(layers,tibble(layer=i,attribute='date',value=paste0(substr(date,9,10),'/',substr(date,6,7),'/',substr(date,1,4))))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Multiplex_Petipa1970/layers.csv')

#interactions & nodes
interactions<-NULL
nodes<-NULL
for(i in 1:2){
  interactions<-rbind(interactions,tibble(node_from=datasets[[i]]$interactions$node_from,layer_from=i,
                                          node_to=datasets[[i]]$interactions$node_to,layer_to=i,
                                          weight=1,type=datasets[[i]]$interactions$type))
  
  nodes<-rbind(nodes,tibble(node_id=datasets[[i]]$nodes$node_id,node_name=datasets[[i]]$nodes$original_name,
                            type=datasets[[i]]$nodes$node_level,
                            attribute1="taxonomy_name",value1=datasets[[i]]$nodes$taxonomy.name,
                            attribute2="taxonomy_rank",value2=datasets[[i]]$nodes$taxonomy.rank))
}
for(i in 1:nrow(interactions)){
  interactions$node_from[i]<-filter(nodes,node_id==interactions$node_from[i])$node_name
  interactions$node_to[i]<-filter(nodes,node_id==interactions$node_to[i])$node_name
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Multiplex_Petipa1970/interactions.csv')

nodes<-nodes %>% distinct(node_name, .keep_all=TRUE)
nodes$node_id<-1:nrow(nodes)
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Multiplex_Petipa1970/nodes.csv')

#references
references<-tibble(doi=NA,author="T. S. Petipa, E. V. Pavlova, G. N. Mironov",year="1970",
                   attribute1="title",value1="The food web structure, utilization transport of energy by trophic levels in the planktonic communities",
                   attribute2="booktitle",value2="Marine Food Chains")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Multiplex_Petipa1970/references.csv')



