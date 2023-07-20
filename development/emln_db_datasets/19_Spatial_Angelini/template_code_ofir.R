library(tidyverse)
library(bipartite)

datasets<-list()
for(i in 1:2){
  name<-paste0("FW_00",i,".csv")
  datasets[[i]]<-read.csv((name))
  datasets[[i]][is.na(datasets[[i]])]<-0
}
reference_data<-read.csv("references_data.csv")

#layers
layers=NULL
years<-c("1992-1995","2003-2005")
for(i in 1:2){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=reference_data$Locality.of.Studi[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=reference_data$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=reference_data$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_species',value=reference_data$Species[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=reference_data$Interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='connectance',value=reference_data$Connectance[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='year',value=years[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Angelini/layers.csv')

#interactions
generate_table<-function(data,layer){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 2:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        output<-rbind(output,tibble(node_from=data$X[[i]],layer_from=layer,node_to=colnames(data)[[j]],layer_to=layer,weight=square,type="trophic"))
      }
    }
  }
  return(output)
}
interactions=NULL
for(i in 1:2){
  interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}

##noa add 13/4/23
interactions <- as.data.frame(apply(interactions,c(1,2),gsub, pattern='\\.$',replacement=''))
interactions[,c('node_from','node_to')] <- as.data.frame(apply(interactions[,c('node_from','node_to')],c(1,2),gsub, pattern='\\.',replacement=' '))

interactions <- cbind(interaction_id=1:nrow(interactions),as.data.frame(apply(interactions,c(1,2),gsub, pattern=' ',replacement='_')))
#interaction 127 should be from and to with the same node id

prey <-unique(interactions$node_from)
predators <- unique(interactions$node_to)
no <- unique(c(predators,prey))
nodes <- NULL
for (i in 1:length(no)) {
  nodes <- rbind(nodes,data.frame(node_id=i, node_name=no[i], type='organism'))
}

interactions <- pivot_longer(interactions,!interaction_id, names_to = 'attribute', values_to = 'value')

write_csv(interactions, 'interactions.csv')

nodes <- pivot_longer(nodes,!node_id, names_to = 'attribute', values_to = 'value')


write_csv(nodes,'nodes.csv')

#nodes
organisms<-unique(interactions$node_from,interactions$node_to)
organisms<-unique(organisms)
nodes<-tibble(node_id=1:length(organisms),node_name=organisms,type="organism")
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets//Spatial_Angelini/nodes.csv')

#references
references<-tibble(doi="10.1016/j.ecolmodel.2004.06.025",author="Ronaldo Angelini",year="20/01/2005",paper_url="https://www.sciencedirect.com/science/article/pii/S0304380004003606",ISSN=NA)
references<-rbind(references,tibble(doi=NA,author="Ronaldo Angelini",year="01/01/2011",paper_url="https://www.researchgate.net/publication/287728990_Mixed_food_web_control_and_stability_in_a_Cerrado_river_Brazil",ISSN='1809-9009'))
write_csv(references, '~/GitHub/emln_db/EMLN_Datasets/ofir_segev/Spatial_Angelini/references.csv')


#noa fix
nodes <- read_csv('nodes.csv') %>% pivot_wider(id_cols = node_id, names_from = attribute, values_from = value)
interactions <- read_csv('interactions.csv') %>% pivot_wider(id_cols = interaction_id, names_from = attribute, values_from = value)

nodes$node_name %in% unique(c(interactions$node_from,interactions$node_to))




