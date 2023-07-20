library(tidyverse)
library(bipartite)

datasets<-list()
for(i in 32:33){
  name<-paste0("M_SD_0",i,".csv")
  datasets[[i-31]]<-read.csv((name))
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
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Poulin1999/layers.csv')

#interactions
generate_table<-function(data,layer){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 2:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        output<-rbind(output,tibble(node_from=data$X[[i]],layer_from=layer,node_to=colnames(data)[[j]],layer_to=layer,weight=square,type="seed-dispersal"))
      }
    }
  }
  return(output)
}
interactions=NULL
for(i in 1:2){
  interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Poulin1999/interactions.csv')

#nodes
plants<-unique(interactions$node_from)
birds<-unique(interactions$node_to)
nodes<-tibble(node_id=1:(length(plants)+length(birds)),node_name=c(plants,birds),
              type=c(rep("plant",length(plants)),rep("bird",length(birds))))
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Poulin1999/nodes.csv')

#references
references<-tibble(doi="10.1017/S0266467499000760",author="Brigitte Poulin",year="01/03/1999")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Poulin1999/references.csv')





