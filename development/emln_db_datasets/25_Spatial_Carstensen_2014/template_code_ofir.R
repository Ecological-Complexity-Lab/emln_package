library(tidyverse)
library(bipartite)
source("~/GitHub/emln_db/data_structure_functions.R")

data<-read.csv("Carstensen2014.csv")

#layers
spacial_layers<-unique(data$Site)
layers<-NULL
n_interactions<-c(135,159,91,115,175,159,122)
pollinators<-c(69,76,59,68,85,71,53)
plants<-c(27,33,22,23,26,31,25)
altitudes<-c(1121,1181,1188,1095,1073,1269,1205)
latitudes<-c(-19.232078,-19.293453,-19.220725,-19.272803,-19.258089,-19.247308,-19.270297)
longtitudes<-c(-43.576394,-43.555533,-43.582964,-43.579836,-43.583869,-43.510197,-43.550353)
for(i in 1:length(spacial_layers)){
  layers<-rbind(layers,tibble(layer=i,attribute='name',value=spacial_layers[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='altitude',value=altitudes[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=latitudes[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=longtitudes[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=n_interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='pollinators',value=pollinators[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='plants',value=plants[i]))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Carstensen2014/layers.csv')

#interactions
interactions<-tibble(node_from=data$Plant.species,layer_from=data$Site,node_to=data$Pollinator.species,layer_to=data$Site,weight=data$Visits,
                     type="pollination",attribute1="date",value1=data$Date)
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Carstensen2014/interactions.csv')

#nodes
plants_unique <- unique(data$Plant.species)
pollinators_unique <- unique(data$Pollinator.species)
nodes <- tibble(node_id=1:(length(pollinators_unique)+length(plants_unique)),
                    node_name=c(pollinators_unique,plants_unique),
                    type=c(rep('plant',length(plants_unique)),rep('pollinator',length(pollinators_unique))))
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Carstensen2014/nodes.csv')

#references
references<-tibble(doi="10.1371/journal.pone.0112903",author="Daniel W. Carstensen",year="10/11/2014")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Carstensen2014/references.csv')
