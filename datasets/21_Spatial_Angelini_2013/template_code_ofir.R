library(tidyverse)
library(bipartite)

datasets<-list()
datasets[[1]]<-read.csv("FW_009.csv")
datasets[[2]]<-read.csv("FW_010.csv")
for(i in 1:2){
  datasets[[i]][is.na(datasets[[i]])]<-0
}
reference_data<-read.csv("references_data.csv")

#layers
layers=NULL
for(i in 1:2){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=reference_data$Locality.of.Studi[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=reference_data$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=reference_data$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_species',value=reference_data$Species[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=reference_data$Interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='connectance',value=reference_data$Connectance[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Angelini2013/layers.csv')

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
  return(output)
}
interactions=NULL
for(i in 1:2){
  interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Angelini2013/interactions.csv')

#nodes
organisms<-unique(interactions$node_from,interactions$node_to)
organisms<-unique(organisms)
nodes<-tibble(node_id=1:length(organisms),node_name=organisms,type="organism")
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Angelini2013/nodes.csv')

references<-tibble(doi="10.1016/j.ecolmodel.2013.01.001",author="Ronaldo Angelini",year="24/03/2013")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Angelini2013/references.csv')











