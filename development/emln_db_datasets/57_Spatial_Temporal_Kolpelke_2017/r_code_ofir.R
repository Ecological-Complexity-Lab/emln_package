library(tidyverse)
library(bipartite)
library("rmangal")

datasets<-search_datasets(list(ref_id = 6))
datasets<-get_collection(datasets)

datasets <- datasets[-layers_to_omit] ##geting rid of the networks in the datasets that have no matrix


#layers
layers=NULL
for(i in 1:length(datasets)) {
  layers<-rbind(layers,tibble(layer=i,attribute='type',value=c('space','time')))
  location<-datasets[[i]]$network$description
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=substr(location,76,nchar(location)-8)))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=datasets[[i]]$network$geom_lat[[1]]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=datasets[[i]]$network$geom_lon[[1]]))
  layers<-rbind(layers,tibble(layer=i,attribute='date',value=as.character(as.Date(datasets[[i]]$network$date))))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Temporal_Kolpelke2017/layers.csv')



#interactions & nodes
# interactions<-NULL
# nodes<-NULL
# for(i in 1:783){
#   if(!is.na(datasets[[i]]$interactions$interaction_id)){
#     interactions<-rbind(interactions,tibble(node_from=datasets[[i]]$interactions$node_from,layer_from=i,
#                                             node_to=datasets[[i]]$interactions$node_to,layer_to=i,
#                                             weight=datasets[[i]]$interactions$value,type=datasets[[i]]$interactions$type,
#                                             attribute1="method",value1=datasets[[i]]$interactions$method))
#   }
#   if(i==61){
#     nodes<-rbind(nodes,tibble(node_id=datasets[[i]]$nodes$node_id,node_name=datasets[[i]]$nodes$original_name,
#                               type=datasets[[i]]$nodes$node_level,
#                               attribue1="taxonomy_name",value1=NA,
#                               attribue2="taxonomy_rank",value2=NA))
#   }
#   else{
#     nodes<-rbind(nodes,tibble(node_id=datasets[[i]]$nodes$node_id,node_name=datasets[[i]]$nodes$original_name,
#                               type=datasets[[i]]$nodes$node_level,
#                               attribute1="taxonomy_name",value1=datasets[[i]]$nodes$taxonomy.name,
#                               attribute2="taxonomy_rank",value2=datasets[[i]]$nodes$taxonomy.rank))
#   }
# }
# v<-interactions$type=="unspecified"
# interactions$type[v]<-"trophic"
# v<-interactions$type=="parasitism"
# interactions$type[v]<-"host-parasite"
# for(i in 1:nrow(interactions)){
#   interactions$node_from[i]<-filter(nodes,node_id==interactions$node_from[i])$node_name
#   interactions$node_to[i]<-filter(nodes,node_id==interactions$node_to[i])$node_name
# }
# write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Temporal_Kolpelke2017/interactions.csv')
#
# nodes<-nodes %>% distinct(node_name, .keep_all=TRUE)
# nodes$node_id<-1:nrow(nodes)
# write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Temporal_Kolpelke2017/nodes.csv')

#references
references<-tibble(doi=datasets[[1]]$reference$doi,author=datasets[[1]]$reference$first_author,
                                    year=datasets[[1]]$reference$year,paper_url=datasets[[1]]$reference$paper_url)
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Temporal_Kolpelke2017/references.csv')



#######interactions fix ########
inter <- NULL
inter_id_temp <- 0
layers_to_omit <- NULL
for (i in 1:length(datasets)) {
  for (j in 1:length(datasets[[i]][["interactions"]][["interaction_id"]])) {
    if(!is.na(datasets[[i]]$interactions$interaction_id)) {
      inter_id_temp <- inter_id_temp+1
      inter <- rbind(inter, data.frame(interaction_id =  inter_id_temp, attribute = 'node_from', value = gsub(datasets[[i]][["nodes"]][datasets[[i]][["nodes"]]$node_id == datasets[[i]][["interactions"]][["node_from"]][j],]$original_name,pattern = ' ',replacement = '_'))) #get the node_name from the node_id
      inter <- rbind(inter, data.frame(interaction_id =  inter_id_temp, attribute = 'layer_from', value = i))
      inter <- rbind(inter, data.frame(interaction_id =  inter_id_temp, attribute = 'node_to', value = gsub(datasets[[i]][["nodes"]][datasets[[i]][["nodes"]]$node_id == datasets[[i]][["interactions"]][["node_to"]][j],]$original_name,pattern = ' ',replacement = '_')))
      inter <- rbind(inter, data.frame(interaction_id =  inter_id_temp, attribute = 'layer_to', value = i))
      inter <- rbind(inter, data.frame(interaction_id =  inter_id_temp, attribute = 'type', value = 'host-parasite'))
      inter <- rbind(inter, data.frame(interaction_id =  inter_id_temp, attribute = 'method', value = 'field_observation'))

    } else {
      layers_to_omit <- c(layers_to_omit,i)
    }
  }
}




###### nodes fix #######

#from = parasite
#to = host

n <- NULL
for (i in 1:length(datasets)) {
  if (i == 60) {
    next
  }
  n <- rbind(n, select(datasets[[i]][["nodes"]],'node_level', 'original_name', 'taxonomy.name', 'taxonomy.rank'))
}

n1 <- distinct(n, original_name, .keep_all = TRUE)

n1$original_name <- gsub(n1$original_name, pattern = ' ',replacement = '_')
n1$type <- NA

##determine node types
del <- NULL #if there are nodes that dont have interactions, this will hold their index
for (i in 1:nrow(n1)) {
  if (n1$original_name[[i]] %in% inter[inter$attribute == 'node_from',]$value) {
    n1$type[[i]] <- 'parasite'
  } else if (n1$original_name[[i]] %in% inter[inter$attribute == 'node_to',]$value) {
    n1$type[[i]] <- 'host'
  } else if (n1$original_name[[i]] %in% inter[inter$attribute == 'node_from',]$value &n1$original_name[[i]] %in% inter[inter$attribute == 'node_to',]$value){
    print(i)
  } else {
    del <- c(del,i)
  }
    }

n1 <- n1[-del,]
colnames(n1) <- gsub(colnames(n1),pattern = '\\.',replacement = '_')

n1 <- rename(n1, 'node_name' = original_name)
n1 <- cbind(data.frame(node_id = 1:nrow(n1)), n1)
n1 <- n1[, c(1, 3, 6, 2, 4, 5)]
nodes <- pivot_longer(n1, !node_id, names_to = 'attribute', values_to = 'value')
nodes$value <- gsub(nodes$value, pattern = ' ', replacement = '_')



write_csv(nodes,'nodes.csv')
write_csv(inter,'interactions.csv')
write_csv(layers, 'layers.csv')


taxa_verification_parse(list(getwd()))




#noa_fix
layers <- read_csv('layers.csv') %>% pivot_wider(id_cols = layer, names_from = attribute, values_from = value) %>% tidyr::unnest(cols = everything())
layers <- layers[,-1]
layers <- distinct(layers)
layers <- cbind(layer=1:nrow(layers),layers)

layers <- layers %>%
  pivot_longer(!layer,names_to = 'attribute', values_to = 'value')

layers %>% group_by(date,location) %>% summarise()

write_csv(layers,'layers.csv')

