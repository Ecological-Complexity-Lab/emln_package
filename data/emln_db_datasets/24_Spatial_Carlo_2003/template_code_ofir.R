library(tidyverse)
library(bipartite)

datasets<-list()
for(i in 3:6){
  name<-paste0("M_SD_00",i,".csv")
  datasets[[i-2]]<-read.csv((name))
}
reference_data<-read.csv("references_data.csv")

#layers
layers=NULL
for(i in 1:4){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=reference_data$Locality.of.Studi[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=reference_data$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=reference_data$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_species',value=reference_data$Species[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=reference_data$Interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='connectance',value=reference_data$Connectance[i]))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Carlo2003/layers.csv')

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
for(i in 1:4){
  interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Carlo2003/interactions.csv')

#nodes
plants<-unique(interactions$node_from)
birds<-unique(interactions$node_to)
nodes<-tibble(node_id=1:(length(plants)+length(birds)),node_name=c(plants,birds),
              type=c(rep("plant",length(plants)),rep("bird",length(birds))))
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Carlo2003/nodes.csv')

#references
references<-tibble(doi="10.1007/s10530-012-0357-z",author="Ruben H. Heleno",year="27/11/2012")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Carlo2003/references.csv')
