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

write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Thompson2003/interactions.csv')

#nodes
species_table<-NULL
for(i in 1:7){
 species_table<-rbind(species_table,species[[i]])
}

organisms<-unique(interactions$node_from,interactions$node_to)
organisms<-unique(organisms)
nodes<-tibble(node_id=1:length(organisms),node_name=organisms,type="organism")

write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Thompson2003/nodes.csv')

#references
references<-tibble(doi="10.1890/0012-9658(2003)084[0145:IOSFWO]2.0.CO;2",author="R. M. Thompson",year="01/01/2003")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Thompson2003/references.csv')




