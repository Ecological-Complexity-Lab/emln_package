library(tidyverse)
library(bipartite)
library("rmangal")

datasets<-search_datasets(list(ref_id = 75))
datasets<-get_collection(datasets)

#layers
layers=NULL
for(i in 1:141){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='time'))
  location<-datasets[[i]]$network$description
  layers<-rbind(layers,tibble(layer=i,attribute='name',value=substr(location,119,nchar(location)-5)))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value="Central Valley of California"))
  date<-datasets[[i]]$network$date
  layers<-rbind(layers,tibble(layer=i,attribute='date',value=paste0(substr(date,9,10),'/',substr(date,6,7),'/',substr(date,1,4))))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Temporal_Ponisio2017/layers.csv')

#interactions & nodes
interactions<-NULL
nodes<-NULL
for(i in 1:141){
  interactions<-rbind(interactions,tibble(node_from=datasets[[i]]$interactions$node_from,layer_from=i,
                                          node_to=datasets[[i]]$interactions$node_to,layer_to=i,
                                          weight=datasets[[i]]$interactions$value,type="pollination",
                                          attribute1="method",value1=datasets[[i]]$interactions$method))
  
  nodes<-rbind(nodes,tibble(node_id=datasets[[i]]$nodes$node_id,node_name=datasets[[i]]$nodes$original_name,
                            type=datasets[[i]]$nodes$node_level,
                            attribute1="taxonomy_name",value1=datasets[[i]]$nodes$taxonomy.name,
                            attribute2="taxonomy_rank",value2=datasets[[i]]$nodes$taxonomy.rank))
}
v<-interactions$type=="unspecified"
interactions$type[v]<-"trophic"
for(i in 1:nrow(interactions)){
  interactions$node_from[i]<-filter(nodes,node_id==interactions$node_from[i])$node_name
  interactions$node_to[i]<-filter(nodes,node_id==interactions$node_to[i])$node_name
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_Ponisio2017/interactions.csv')

nodes<-nodes %>% distinct(node_name, .keep_all=TRUE)
nodes$node_id<-1:nrow(nodes)
for(i in 1:nrow(nodes)){
  if(nodes$node_name[i] %in% interactions$node_to)
    nodes$type[i]<-"pollinator"
  else if(nodes$node_name[i] %in% interactions$node_from)
    nodes$type[i]<-"plant"
}
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_Ponisio2017/nodes.csv')

#references
references<-tibble(doi=datasets[[1]]$reference$doi,author=datasets[[1]]$reference$first_author,
                   year=datasets[[1]]$reference$year,paper_url=datasets[[1]]$reference$paper_url)
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Temporal_Ponisio2017/references.csv')


test<-NULL
for(i in 1:141){
 test<-rbind(test,datasets[[i]]$network) 
}


test2<-datasets[[7]]$interactions
substr(test2,119,nchar(test2)-5)
