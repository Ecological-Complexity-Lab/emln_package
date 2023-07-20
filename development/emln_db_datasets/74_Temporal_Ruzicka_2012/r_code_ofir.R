library(tidyverse)
library(bipartite)
library("rmangal")

datasets<-search_datasets(list(ref_id = 79))
datasets<-get_collection(datasets)

#layers
layers=NULL
for(i in 1:5){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='time'))
  layers<-rbind(layers,tibble(layer=i,attribute='name',value="Northern California Current"))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value="42, 42, 48.34, 48.34, 42, 42"))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value="-126, -124, -124, -126, -126, -126"))
  date<-datasets[[i]]$network$date
  layers<-rbind(layers,tibble(layer=i,attribute='date',value=paste0(substr(date,9,10),'/',substr(date,6,7),'/',substr(date,1,4))))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_Ruzicka2012/layers.csv')

#interactions & nodes
interactions<-NULL
nodes<-NULL
for(i in 1:5){
  interactions<-rbind(interactions,tibble(node_from=datasets[[i]]$interactions$node_from,layer_from=i,
                                          node_to=datasets[[i]]$interactions$node_to,layer_to=i,
                                          weight=datasets[[i]]$interactions$value,type=datasets[[i]]$interactions$type,
                                          attribute1="method",value1=datasets[[i]]$interactions$method,
                                          attribute2="weight calculation",value2="Proportions of the consumer diet"))
  nodes<-rbind(nodes,tibble(node_id=datasets[[i]]$nodes$node_id,node_name=datasets[[i]]$nodes$original_name,
                            type=datasets[[i]]$nodes$node_level,
                            attribute1="taxonomy_name",value1=datasets[[i]]$nodes$taxonomy.name,
                            attribute2="taxonomy_rank",value2=datasets[[i]]$nodes$taxonomy.rank))
}
v<-interactions$type=="unspecified"
interactions$type[v]<-"trophic"
for(i in 1:nrow(nodes)){
  name<-nodes$node_name[i]
  index<-str_locate(pattern = ':',name)[1]+1
  if(!is.na(index))
    nodes$node_name[i]<-substr(name,index,nchar(name))
}
for(i in 1:nrow(interactions)){
  interactions$node_from[i]<-filter(nodes,node_id==interactions$node_from[i])$node_name
  interactions$node_to[i]<-filter(nodes,node_id==interactions$node_to[i])$node_name
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Temporal_Ruzicka2012/interactions.csv')

nodes<-nodes %>% distinct(node_name, .keep_all=TRUE)
nodes$node_id<-1:nrow(nodes)
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_Ruzicka2012/nodes.csv')

#references
references<-tibble(doi=datasets[[1]]$reference$doi,author=datasets[[1]]$reference$first_author,
                   year=datasets[[1]]$reference$year,paper_url=datasets[[1]]$reference$paper_url)
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Temporal_Ruzicka2012/references.csv')

test<-rbind(datasets[[1]]$network,datasets[[2]]$network,datasets[[3]]$network,datasets[[4]]$network,datasets[[5]]$network)
test2<-datasets[[1]]$nodes
test2<-str_locate(pattern = ':',"bul")[1]+1
test2[1]
is.na(test2)



