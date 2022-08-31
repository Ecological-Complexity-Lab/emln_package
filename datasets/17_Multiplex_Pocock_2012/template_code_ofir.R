library(tidyverse)
library(bipartite)
source("~/GitHub/emln_db/data_structure_functions.R")

data<-read.csv("Pocock2012_norwood.csv")
#layers
guilds<-unique(data[c("lower.guild","upper.guild")])
layers<-tibble(layer_id=1:nrow(guilds),layer_name=paste0(guilds$lower.guild,' - ',guilds$upper.guild),
               description='lower guild - upper guild',type='interaction',directed=FALSE)

layers<-NULL
for(i in 1:nrow(guilds)){
  layers<-rbind(layers,tibble(layer=i,attribute='name',value=paste0(guilds$lower.guild[i],' - ',guilds$upper.guild[i])))
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='interaction'))
}
write_csv(layers, '~/GitHub/emln_db/EMLN_Datasets/ofir/Multiplex_Pocock2012/layers.csv')

#interactions
interactions<-tibble(node_from=data$lower.taxon,layer_from=paste0(data$lower.guild,' - ',data$upper.guild),
              node_to=data$upper.taxon, layer_to=paste0(data$lower.guild,' - ',data$upper.guild),
              weight=data$estimated.interaction.strength)
mutualism<- c('plant - flower visitor','plant - butterfly')
parasitism<-c('aphid - primary aphid parasitoid','plant - aphid','plant - primary aphid parasitoid','plant - rodent ectoparasite','plant - secondary aphid parasitoid','plant - seed-feeder parasitoid',
                'plant - leaf-miner parasitoid','seed-feeding insect - seed-feeder parasitoid','seed-feeding rodent - rodent ectoparasite','aphid - secondary aphid parasitoid')
trophism<-c('plant - seed-feeding bird','plant - seed-feeding insect','plant - seed-feeding rodent')
interactions<-cbind(interactions,tibble(type=case_when(interactions$layer_from %in% mutualism ~ "pollination",
                                                       interactions$layer_from %in% parasitism ~ "host-parasite",
                                                       interactions$layer_from %in% trophism ~ "trophic")))
write_csv(interactions, '~/GitHub/emln_db/EMLN_Datasets/ofir/Multiplex_Pocock2012/interactions.csv')

#nodes
lower_guild<-unique(data[c("lower.guild","lower.taxon")])
upper_guild<-unique(data[c("upper.guild","upper.taxon")])
nodes<-tibble(node_name=c(lower_guild$lower.taxon,upper_guild$upper.taxon),type=c(lower_guild$lower.guild,upper_guild$upper.guild))
nodes<- unique(nodes[c("node_name","type")])
nodes<- cbind(node_id=1:nrow(nodes),nodes)
write_csv(nodes, '~/GitHub/emln_db/EMLN_Datasets/ofir/Multiplex_Pocock2012/nodes.csv')

#references
references<-tibble(doi="10.1126/science.1214915",author="Michael J. O. Pocock",year="24/02/2012")
write_csv(references, '~/GitHub/emln_db/EMLN_Datasets/ofir/Multiplex_Pocock2012/references.csv')
