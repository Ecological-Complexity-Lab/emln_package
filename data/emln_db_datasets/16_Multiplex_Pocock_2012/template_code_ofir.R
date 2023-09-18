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


# Noa Revision ------------------------------------------------------------


nodes <- read_csv('nodes.csv')
nodes %<>%
  distinct() %>%
  pivot_wider(names_from = 'attribute', values_from = 'value')
nodes$node_name[which(duplicated(nodes$node_name))]
# nodes <- as.data.frame(apply(apply(nodes, c(1,2), trimws),c(1,2),gsub,pattern=' ',replacement='_'))

#if node is in lower and upper guild, then it has multiple types
colnames(lower_guild) <- c('type','name')
colnames(upper_guild) <- c('type','name')
lower_guild$name[grep(lower_guild$name,pattern = 'Triticosecale')] <- 'Triticosecale'
guild <- rbind(lower_guild,upper_guild)
guild <- distinct(as.data.frame(apply(guild,c(1,2), gsub, pattern=' |[[:punct:]]',replacement = '_')))
nodes_updated <- NULL
for (i in 1:length(unique(guild$name))) {
  name <- unique(guild$name)[i]
  nodes_updated<-rbind(nodes_updated,tibble(node_id=i,node_name=name,type=guild[guild$name==name,]$type))
}

#grep(unique(guild$name),pattern = paste0('^',i,'$'))

nodes_updated <- cbind(nodes_updated,data.frame(taxa_verified=NA,verification_level=NA))
#add taxize data
# nodes <- as.data.frame(apply(nodes,c(1,2),trimws))
# nodes <- as.data.frame(apply(nodes,c(1,2),gsub, pattern='[[:punct:]]', replacement='_'))
# nodes$node_name[grep(nodes$node_name,pattern = 'Triticosecale')] <- 'Triticosecale'
for (i in nodes_updated$node_name) {
  nodes_updated[nodes_updated$node_name==i,c('taxa_verified','verification_level')] <- nodes[nodes$attribute %in% c('taxa_verified','verification_level') & nodes$value == i,] %>%
    filter(attribute %in% c('node_name','taxa_verified','verification_level'), value == i)
}



interactions <- read_csv('interactions.csv')
interactions %<>% pivot_wider(id_cols = interaction_id, names_from = 'attribute', values_from = 'value')
interactions$node_from <- gsub(x = interactions$node_from,pattern='[[:punct:]]', replacement='_')
interactions$node_to <- gsub(x = interactions$node_to,pattern='[[:punct:]]', replacement='_')
interactions %<>% pivot_longer(!interaction_id, names_to = 'attribute', values_to = 'value')

interactions$value[grep(interactions$value,pattern = 'Triticosecale')] <- 'Triticosecale'

inter_names <- interactions %>%
  filter(attribute%in%c('node_from','node_to')) %>%
  select(value) %>%
  unique() %>%
  unlist()

inter_names[!inter_names %in% nodes_updated$node_name]

nodes_updated %<>% pivot_longer(!node_id, names_to = 'attribute', values_to = 'value')

write_csv(nodes_updated,'nodes.csv')
write_csv(interactions,'interactions.csv')

which(duplicated(data.frame(nodes$node_id,nodes$node_name)))



write_csv(nodes_updated,'nodes.csv')

nodes_updated <- read_csv('nodes.csv')
nodes_updated <- nodes_updated[!nodes_updated$node_id>549,]


#delete type
nodes <- read_csv('nodes_w_type.csv')
write_csv(nodes, 'nodes_w_type.csv')
nodes <- nodes %<>%
  tidyr::pivot_wider(names_from = 'attribute',values_from = 'value', values_fn = list) %>%
  tidyr::unnest(cols = everything()) %>%
  select(-type) %>%
  distinct() %>%
  pivot_longer(!node_id, names_to = 'attribute', values_to = 'value');nodes

write_csv(nodes, 'nodes.csv')
