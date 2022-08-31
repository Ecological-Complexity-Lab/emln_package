library(tidyverse)
library(bipartite)
library("rmangal")

datasets<-search_datasets(list(ref_id = 145))
datasets<-get_collection(datasets)

#layers
layers=NULL
for(i in 1:11){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='environment'))
  dis<-datasets[[i]]$network$description
  index<-str_locate(pattern=",",dis)
  layers<-rbind(layers,tibble(layer=i,attribute='habitat',value=substr(dis,27,index[1]-1)))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value="southern California, USA"))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value="33.304209435313, 34.585735297605, 34.987090779836, 33.661200052419, 33.304209435313"))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value="-116.114402698622, -117.813621336863, -116.954512936205, -116.003482299683, -116.114402698622"))
  date<-datasets[[i]]$network$date
  layers<-rbind(layers,tibble(layer=i,attribute='date',value=paste0(substr(date,9,10),'/',substr(date,6,7),'/',substr(date,1,4))))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Hawkins_Goeden1984/layers.csv')

#interactions & nodes
interactions<-NULL
nodes<-NULL
for(i in 1:11){
  interactions<-rbind(interactions,tibble(node_from=datasets[[i]]$interactions$node_from,layer_from=i,
                                          node_to=datasets[[i]]$interactions$node_to,layer_to=i,
                                          weight=datasets[[i]]$interactions$value,type=datasets[[i]]$interactions$type,
                                          attribute1="method",value1=datasets[[i]]$interactions$method))
  
  nodes<-rbind(nodes,tibble(node_id=datasets[[i]]$nodes$node_id,node_name=datasets[[i]]$nodes$original_name,
                            type=datasets[[i]]$nodes$node_level,
                            attribute1="taxonomy_name",value1=datasets[[i]]$nodes$taxonomy.name,
                            attribute2="taxonomy_rank",value2=datasets[[i]]$nodes$taxonomy.rank))
}
v<-interactions$type=="unspecified"
interactions$type[v]<-"trophic"
v<-interactions$type=="parasitism"
interactions$type[v]<-"host-parasite"
for(i in 1:nrow(interactions)){
  interactions$node_from[i]<-filter(nodes,node_id==interactions$node_from[i])$node_name
  interactions$node_to[i]<-filter(nodes,node_id==interactions$node_to[i])$node_name
}
write_csv(interactions, '~/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Hawkins_Goeden1984/interactions.csv')

nodes<-nodes %>% distinct(node_name, .keep_all=TRUE)
nodes$node_id<-1:nrow(nodes)
write_csv(nodes, '~/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Hawkins_Goeden1984/nodes.csv')

#references
references<-tibble(doi=datasets[[1]]$reference$doi,author=datasets[[1]]$reference$first_author,
                   year=datasets[[1]]$reference$year,paper_url=datasets[[1]]$reference$paper_url)
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Hawkins_Goeden1984/references.csv')

