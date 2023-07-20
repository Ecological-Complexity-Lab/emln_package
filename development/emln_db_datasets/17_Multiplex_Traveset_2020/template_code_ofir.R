library(tidyverse)
library(bipartite)

data<-read_delim("Hervias et al 2020 - Species functional traits and abundance as drivers of multiplex ecological networks.csv", delim = ';')
sc_inter<-read.csv("inter_edges_SC_edgelist.csv")
sx_inter<-read.csv("inter_edges_SX_edgelist.csv")
sc_poll<-read.csv("intra_pollSC_edgelist.csv")
sx_poll<-read.csv("intra_pollSX_edgelist.csv")
sc_seed<-read.csv("intra_seedSC_edgelist.csv")
sx_seed<-read.csv("intra_seedSX_edgelist.csv")

#layers
layers<-NULL
names<-c("pollination","seed-dispersal","pollination","seed-dispersal")
locations<-c("San Cristobal","San Cristobal","Santa Cruz","Santa Cruz")
latitudes<-c(-0.909114,-0.909114,-0.533386,-0.533386)
longtitudes<-c(-89.561852,-89.561852,-90.350216,-90.350216)
for(i in 1:4){
  layers<-rbind(layers,tibble(layer=i,attribute='name',value=paste0(names[i],'_',locations[i])))
  layers<-rbind(layers,tibble(layer=i,attribute='type',value="interaction"))
  layers<-rbind(layers,tibble(layer=i,attribute='interaction_type',value=names[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=paste0(locations[i],",Galapagos")))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=latitudes[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=longtitudes[i]))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Multiplex_Traveset_2020/layers.csv')

#interactions
interactions<-tibble(node_from=sc_poll$plant_sp,layer_from="pollination_San Cristobal",node_to=sc_poll$bird_sp,
                     layer_to="pollination_San Cristobal",weight=sc_poll$intralayer,type="pollination")
interactions<-rbind(interactions,tibble(node_from=sx_poll$plant_sp,layer_from="polination_Santa Cruz",node_to=sx_poll$bird_sp,
                     layer_to="pollination_Santa Cruz",weight=sx_poll$intralayer,type="pollination"))
interactions<-rbind(interactions,tibble(node_from=sc_seed$plant_sp,layer_from="seed-dispersal_San Cristobal",node_to=sc_seed$bird_sp,
                                        layer_to="seed-dispersal_San Cristobal",weight=sc_seed$intralayer,type="seed-dispersal"))
interactions<-rbind(interactions,tibble(node_from=sx_seed$plant_sp,layer_from="seed-dispersal_Santa Cruz",node_to=sx_seed$bird_sp,
                                        layer_to="seed-dispersal_Santa Cruz",weight=sx_seed$intralayer,type="seed-dispersal"))
interactions<-rbind(interactions,tibble(node_from=sc_inter$species,layer_from="pollination_San Cristobal",node_to=sc_inter$species,
                                        layer_to="seed-dispersal_San Cristobal",weight=sc_inter$interlayer,type="interlayer"))
interactions<-rbind(interactions,tibble(node_from=sx_inter$species,layer_from="pollination_Santa Cruz",node_to=sx_inter$species,
                                        layer_to="seed-dispersal_Santa Cruz",weight=sx_inter$interlayer,type="interlayer"))
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Multiplex_Traveset_2020/interactions.csv')

#nodes
plants<-unique(c(sc_poll$plant_sp,sx_poll$plant_sp,sc_seed$plant_sp,sx_seed$plant_sp))
birds<-unique(c(sc_poll$bird_sp,sx_poll$bird_sp,sc_seed$bird_sp,sx_seed$bird_sp))
nodes<-tibble(node_id=1:length(c(plants,birds)),node_name=(c(plants,birds)),type=c(rep("plant",length(plants)),rep("bird",length(birds))))
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Multiplex_Traveset_2020/nodes.csv')


#references
references<-tibble(doi="10.1098/rspb.2020.2127",author="S. Hervías-Parejo",year="02/11/2020")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Multiplex_Traveset_2020/references.csv')


#noa fix
interactions <- interactions %>%
pivot_wider(id_cols = interaction_id, names_from = attribute, values_from = value)
interactions <- as.data.frame(apply(interactions, c(1,2), gsub, pattern='polination_Santa', replacement='pollination_Santa'))
interactions <-  interactions %>%
  pivot_longer(!interaction_id, names_to = 'attribute', values_to = 'value')

write_csv(interactions,'interactions.csv')
