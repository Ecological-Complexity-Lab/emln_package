library(tidyverse)
library(bipartite)

networks<-read.csv("Networks.csv")
plant_sp<-distinct(read.csv("Plant_species_list.csv"))
insect_sp<-distinct(read.csv("Insect_species_list.csv"))
visits<-read.csv("Flower_visits.csv")
flower_areas<-read.csv("Flower_areas.csv")


###noa fix
insect_sp <- distinct(as.data.frame(apply(insect_sp, c(1,2), trimws)))
plant_sp <- distinct(as.data.frame(apply(plant_sp, c(1,2), trimws)))
###

#flower areas, flower morphology and weather was not integrated into the data!

#layers
layers=NULL
for(i in 1:68){
  layers<-rbind(layers,tibble(layer=networks$Network[[i]],attribute='type',value='environment'))
  layers<-rbind(layers,tibble(layer=networks$Network[[i]],attribute='location',value="National Park Berchtesgaden, Germany"))
  layers<-rbind(layers,tibble(layer=networks$Network[[i]],attribute='plot',value=networks$Plot[[i]]))
  layers<-rbind(layers,tibble(layer=networks$Network[[i]],attribute='altitude_m',value=networks$Altitude_m[[i]]))
  layers<-rbind(layers,tibble(layer=networks$Network[[i]],attribute='latitude',value=47.552571))
  layers<-rbind(layers,tibble(layer=networks$Network[[i]],attribute='longitude',value=12.925284))
  layers<-rbind(layers,tibble(layer=networks$Network[[i]],attribute='observation_time',value=networks$Observation_time[[i]]))
}
write_csv(layers, '~/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Benadi_JAE_2013/layers.csv')

#interactions
interactions<-tibble(node_from=visits$Insect_species, layer_from=visits$Network_no, node_to=visits$Plant_species, layer_to=visits$Network_no,
                     weight=1, type="pollination", attribute1="date", value1=visits$Date,
                     attribute2="time", value2=visits$Time)
write_csv(interactions, '~/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Benadi_JAE_2013/interactions.csv')

#nodes
nodes<-tibble(node_name=gsub(plant_sp$Species,pattern = ' ',replacement = '_'),type="plant",family=plant_sp$Family,
              color=plant_sp$Colour, kugler_type=plant_sp$KuglerType, order=rep(NA))
nodes<-rbind(nodes,tibble(node_name=gsub(insect_sp$Species,pattern = ' ',replacement = '_'),type="pollinator",family=insect_sp$Family, color=rep(NA), kugler_type=rep(NA),
                          order=insect_sp$Order))

##noa fix##
nodes <- nodes %>%
  mutate(attribute4 = 'order', value4 = c(rep.int(NA,166),nodes$value2[167:length(nodes$attribute2)])) %>%
  mutate(attribute2 = 'color', value2 = c(nodes$value2[1:166],rep.int(NA,606-166))) %>%
  mutate(attribute3 = nodes$attribute3[1])
nodes<-cbind(tibble(node_id=1:nrow(nodes)),nodes)
inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)

nodes %<>%
  pivot_longer(cols = !node_id,names_to = 'attribute', values_to = 'value')

source('~/GitHub/emln_db/EMLN_Datasets/checks/taxonomic_verification.R')
taxa_verification_parse(list(getwd()))
##noa fix##


write_csv(nodes, 'nodes.csv')


inter <- read_csv('interactions.csv') %>% pivot_wider(id_cols = interaction_id,names_from = attribute, values_from = value)
inter$date <- as.Date(inter$date,
                      format = "%d/%m/%y")
inter %<>% pivot_longer(!interaction_id, names_to = 'attribute', values_to = 'value', values_transform = list(value=as.character))
write_csv(inter,'interactions.csv')

#references
references<-tibble(doi="10.1111/1365-2656.12158",author="Gita Benadi", year="08/10/2013")
write_csv(references, '~/GitHub/emln_db/EMLN_Datasets/ofir/Environment_Benadi_JAE_2013/references.csv')




