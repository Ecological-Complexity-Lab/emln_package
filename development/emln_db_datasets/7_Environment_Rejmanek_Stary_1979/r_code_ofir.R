library(tidyverse)
library(bipartite)
library("rmangal")

datasets<-search_datasets(list(ref_id = 112))
datasets<-get_collection(datasets)

#layers
layers=NULL
for(i in 1:4){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='environment'))
  layers<-rbind(layers,tibble(layer=i,attribute='description',value="system of Ahpid/parasitoid"))
  dis<-datasets[[i]]$network$description
  index<-str_locate(pattern="in",dis)
  layers<-rbind(layers,tibble(layer=i,attribute='habitat',value=substr(dis,31,index[1]-2)))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value="Central Europe"))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=datasets[[i]]$network$geom_lat[[1]]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=datasets[[i]]$network$geom_lon[[1]]))
  date<-datasets[[i]]$network$date
  layers<-rbind(layers,tibble(layer=i,attribute='date',value=paste0(substr(date,9,10),'/',substr(date,6,7),'/',substr(date,1,4))))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '~/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Rejmanek_Stary1979/layers.csv')

#interactions & nodes
interactions<-NULL
nodes<-NULL
for(i in 1:4){
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
write_csv(interactions, '~/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Rejmanek_Stary1979/interactions.csv')

nodes<-nodes %>% distinct(node_name, .keep_all=TRUE)
nodes$node_id<-1:nrow(nodes)
write_csv(nodes, '~/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Rejmanek_Stary1979/nodes.csv')

#references
references<-tibble(doi=datasets[[1]]$reference$doi,author=datasets[[1]]$reference$first_author,
                   year=datasets[[1]]$reference$year,paper_url=datasets[[1]]$reference$paper_url)
write_csv(references, '~/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Rejmanek_Stary1979/references.csv')

