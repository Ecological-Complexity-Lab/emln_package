library(tidyverse)
library(bipartite)
source('~/GitHub/emln_db/data_structure_functions.R')

datasets<-list()
species<-list()
for(i in 1:2){
  name<-paste0("FW_012_0",i,".csv")
  s<-paste0("FW_012_0",i,"-species",".csv")
  datasets[[i]]<-read.csv((name))
  datasets[[i]][is.na(datasets[[i]])]<-0
  species[[i]]<-read.csv((s))
}
for(i in 1:5){
  name<-paste0("FW_013_0",i,".csv")
  s<-paste0("FW_013_0",i,"-species",".csv")
  datasets[[i+2]]<-read.csv((name))
  datasets[[i+2]][is.na(datasets[[i+2]])]<-0
  species[[i+2]]<-read.csv((s))
}
reference_data<-read.csv("references_data.csv")

#layers
layers=NULL
for(i in 1:7){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=reference_data$Locality.of.Studi[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=reference_data$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=reference_data$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_species',value=reference_data$Species[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=reference_data$Interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='connectance',value=reference_data$Connectance[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Thompson2003/layers.csv')

#interactions
generate_table<-function(data,layer){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 2:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        output<-rbind(output,tibble(node_from=colnames(data)[[j]],layer_from=layer,node_to=data$X[[i]],layer_to=layer,weight=square,type="trophic"))
      }
    }
  }
  output$node_from<-gsub("[.]", " ", output$node_from)
  return(output)
}
interactions=NULL
for(i in 1:7){
  interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}

##node_fix



write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Thompson2003/interactions.csv')

#nodes
species_table<-NULL
for(i in 1:7){
 species_table<-rbind(species_table,species[[i]])
}

organisms<-unique(c(interactions$node_from,interactions$node_to))

nodes<-tibble(node_id=1:length(organisms),node_name=organisms,type="organism")

nodes$node_id <- trimws(nodes$node_id)
nodes <- pivot_longer(nodes,!node_id, names_to = 'attribute',values_to = 'value')
nodes <- as.data.frame(apply(nodes,c(1,2),gsub,pattern=' ',replacement='_'))

write_csv(nodes, 'nodes.csv')



#references
references<-tibble(doi="10.1890/0012-9658(2003)084[0145:IOSFWO]2.0.CO;2",author="R. M. Thompson",year="01/01/2003")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Thompson2003/references.csv')




#noa fix
nodes <- read_csv('nodes.csv') %>% pivot_wider(id_cols = node_id, names_from = attribute, values_from = value)
interactions <- read_csv('interactions.csv') %>% pivot_wider(id_cols = interaction_id, names_from = attribute, values_from = value)

all(unique(c(interactions$node_from,interactions$node_to)) %in% nodes$node_name)

nodes$node_name[!nodes$node_name%in%unique(c(interactions$node_from,interactions$node_to))]



nodes$node_name <- gsub(nodes$node_name, pattern = '_FW_[0-9]+_[0-9]+',replacement = '')
nodes <- nodes[,-1]
nodes <- distinct(nodes)
nodes <- cbind(node_id=1:nrow(nodes),nodes)
nodes %<>% pivot_longer(!node_id, names_to = 'attribute', values_to = 'value')

write_csv(nodes,'nodes.csv')


interactions$node_from <- gsub(interactions$node_from, pattern = '_FW_[0-9]+_[0-9]+',replacement = '')
interactions$node_to <- gsub(interactions$node_to, pattern = '_FW_[0-9]+_[0-9]+',replacement = '')
interactions%<>%pivot_longer(!interaction_id, names_to = 'attribute', values_to = 'value')

write_csv(interactions,'interactions.csv')


#noa fix

nodes <- read_csv('nodes.csv') %>% pivot_wider(id_cols = node_id, names_from = attribute, values_from = value)
nodes <- nodes[!duplicated(nodes$node_name),]
nodes$node_id <- 1:nrow(nodes)
nodes <- nodes %>% pivot_longer(!node_id, names_to = 'attribute', values_to = 'value')

write_csv(nodes,'nodes.csv')
