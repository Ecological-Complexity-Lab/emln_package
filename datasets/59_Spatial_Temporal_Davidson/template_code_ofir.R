library(tidyverse)
library(bipartite)

datasets<-list()
for(i in 1:2){
  name<-paste0("M_PA_00",i,".csv")
  datasets[[i]]<-read.csv((name))
}
reference_data<-read.csv("references_data.csv")

#layers
layers=NULL
years<-c("1989","1990-1991")
months<-c("September-November","May-August")
for(i in 1:2){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space & time'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=reference_data$Locality.of.Studi[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=reference_data$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=reference_data$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_species',value=reference_data$Species[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=reference_data$Interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='connectance',value=reference_data$Connectance[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='month',value=months[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='year',value=years[i]))
  
}
write_csv(layers, '~/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Temporal_Davidson/layers.csv')

#interactions
generate_table<-function(data,layer){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 2:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        output<-rbind(output,tibble(node_from=colnames(data)[[j]],layer_from=layer,node_to=data$X[[i]],layer_to=layer,weight=square,type="plant-ant"))
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
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Temporal_Davidson/interactions.csv')

#nodes
plants<-unique(interactions$node_from)
ants<-unique(interactions$node_to)
nodes<-tibble(node_id=1:(length(plants)+length(ants)),node_name=c(plants,ants),
              type=c(rep("plant",length(plants)),rep("ant",length(ants))))
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Temporal_Davidson/nodes.csv')

#references
references<-tibble(doi='10.2307/2388444',author="Diane W. Davidson",year="01/03/1989",paper_url="https://www.jstor.org/stable/2388444",data_url=c('http://www.ecologia.ib.usp.br/iwdb/resources.html','http://www.web-of-life.es/map.php?type=11'))
references<-rbind(references,tibble(doi='10.2307/5880',author="Diane W. Davidson",year="01/01/1991",paper_url="https://www.jstor.org/stable/5880",data_url=c('http://www.ecologia.ib.usp.br/iwdb/resources.html','http://www.web-of-life.es/map.php?type=11')))
write_csv(references, '~/GitHub/emln_db/EMLN_Datasets/ofir_segev/Spatial_Temporal_Davidson/references.csv')





