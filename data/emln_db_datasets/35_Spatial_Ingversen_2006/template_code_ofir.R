library(tidyverse)
library(bipartite)

datasets<-list()
for(i in 40:41){
  name<-paste0("M_PL_0",i,".csv")
  datasets[[i-39]]<-read.csv((name))
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
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Ingversen2006/layers.csv')

#interactions
generate_table<-function(data,layer){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 2:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        output<-rbind(output,tibble(node_from=data$X[[i]],layer_from=layer,node_to=colnames(data)[[j]],layer_to=layer,weight=square,type="pollination"))
      }
    }
  }
  return(output)
}
interactions=NULL
for(i in 1:2){
  interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Ingversen2006/interactions.csv')

#nodes
plants<-unique(interactions$node_from)
pollinatiors<-unique(interactions$node_to)
nodes<-tibble(node_id=1:(length(plants)+length(pollinatiors)),node_name=c(plants,pollinatiors),
              type=c(rep("plant",length(plants)),rep("pollinator",length(pollinatiors))))
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Ingversen2006/nodes.csv')

#references
references<-tibble(doi='NA',author="Ingversen TT",year="01/01/2006",data_url = 'http://www.web-of-life.es/map.php')
write_csv(references, '~/GitHub/emln_db/EMLN_Datasets/ofir_segev/Spatial_Ingversen2006/references.csv')





