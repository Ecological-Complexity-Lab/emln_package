library(tidyverse)
library(bipartite)

datasets<-list()
datasets[[1]]<-read.csv(("M_PL_008.csv"))
datasets[[2]]<-read.csv(("M_PL_012.csv"))
reference_data<-read.csv("references_data.csv")

layers=NULL
for(i in 1:2){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space & time'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=reference_data$Locality.of.Studi[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=reference_data$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=reference_data$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_species',value=reference_data$Species[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=reference_data$Interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='connectance',value=reference_data$Connectance[i]))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Olesen/layers.csv')

#interactions
generate_table<-function(data,layer){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 2:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        output<-rbind(output,tibble(node_from=colnames(data)[[j]],layer_from=layer,node_to=data$X[[i]],layer_to=layer,weight=square,type="pollination"))
      }
    }
  }
  output$node_from<-gsub("[.]", " ", output$node_from)
  return(output)
}
interactions=NULL
for(i in 1:2){
  interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Olesen/interactions.csv')

#nodes
plants<-unique(interactions$node_to)
pollinators<-unique(interactions$node_from)
nodes<-tibble(node_id=1:(length(plants)+length(pollinators)),node_name=c(plants,pollinators),
              type=c(rep("plant",length(plants)),rep("pollinator",length(pollinators))))
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Olesen/nodes.csv')

#references
references<-tibble(doi="10.1034/j.1600-0587.2003.03443.x",author="Yoko L. Dupont, Dennis M. Hansen, Jens M. Olesen",year="13/05/2003")
references<-rbind(references,tibble(doi="unpublished",author="Jens M. Olesen",year=NA))
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Olesen/references.csv')





