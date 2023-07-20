#64 and 74 are using the layer and node ids in the interactions csv where they should use the names if there are any -
#here im switching between the ids to the names in the interactions file


##64 -the problem is with the layer ids that are in the interactions
Spatial_Yanez_1978_inter <- read.csv('./64_Spatial_Yanez_1978/interactions.csv')
Spatial_Yanez_1978_layer <- read.csv('./64_Spatial_Yanez_1978/layers.csv')
Spatial_Yanez_1978_nodes <- read.csv('./64_Spatial_Yanez_1978/nodes.csv')

#Spatial_Yanez_1978_inter[Spatial_Yanez_1978_inter$attribute %in% c('layer_from','layer_to'),]$value


load_emln(64)

##DONE - I NEED TO LOAD IT INTO THE PACKAGE


#74 - the problem  is with the node ids - looks like I need to rewrite the interactions csv
Temporal_Pertubation_Kaiser_Bunbury_2009_inter <- read.csv("74_Temporal_Pertubation_Kaiser-Bunbury_2009/interactions.csv")

##typos
Temporal_Pertubation_Kaiser_Bunbury_2009_inter[Temporal_Pertubation_Kaiser_Bunbury_2009_inter$value == 'Ps_ca',]$value <- 'Psi_ca'
Temporal_Pertubation_Kaiser_Bunbury_2009_inter[Temporal_Pertubation_Kaiser_Bunbury_2009_inter$value == 'Ps_te',]$value <- 'Psa_te'

Temporal_Pertubation_Kaiser_Bunbury_2009_layer <- read.csv("74_Temporal_Pertubation_Kaiser-Bunbury_2009/layers.csv")
Temporal_Pertubation_Kaiser_Bunbury_2009_nodes <- read.csv("74_Temporal_Pertubation_Kaiser-Bunbury_2009/nodes.csv")

for (i in 1:max(Temporal_Pertubation_Kaiser_Bunbury_2009_inter$interaction_id)) {

#node_from
  node_from_reference_code <- gsub(x=Temporal_Pertubation_Kaiser_Bunbury_2009_inter[Temporal_Pertubation_Kaiser_Bunbury_2009_inter$interaction_id == i & Temporal_Pertubation_Kaiser_Bunbury_2009_inter$attribute == 'node_from',]$value,pattern = '_',replacement = ' ')
  node_from_id <- Temporal_Pertubation_Kaiser_Bunbury_2009_nodes[which(Temporal_Pertubation_Kaiser_Bunbury_2009_nodes$value == node_from_reference_code),]$node_id
  Temporal_Pertubation_Kaiser_Bunbury_2009_inter[Temporal_Pertubation_Kaiser_Bunbury_2009_inter$interaction_id == i & Temporal_Pertubation_Kaiser_Bunbury_2009_inter$attribute == 'node_from',]$value <- Temporal_Pertubation_Kaiser_Bunbury_2009_nodes[which(Temporal_Pertubation_Kaiser_Bunbury_2009_nodes$node_id == node_from_id & Temporal_Pertubation_Kaiser_Bunbury_2009_nodes$attribute == 'node_name'),]$value

#node_to
  node_to_reference_code <- gsub(x=Temporal_Pertubation_Kaiser_Bunbury_2009_inter[Temporal_Pertubation_Kaiser_Bunbury_2009_inter$interaction_id == i & Temporal_Pertubation_Kaiser_Bunbury_2009_inter$attribute == 'node_to',]$value,pattern = '_',replacement = ' ')
  node_to_id <- Temporal_Pertubation_Kaiser_Bunbury_2009_nodes[which(Temporal_Pertubation_Kaiser_Bunbury_2009_nodes$value == node_to_reference_code),]$node_id
  Temporal_Pertubation_Kaiser_Bunbury_2009_inter[Temporal_Pertubation_Kaiser_Bunbury_2009_inter$interaction_id == i & Temporal_Pertubation_Kaiser_Bunbury_2009_inter$attribute == 'node_to',]$value <- Temporal_Pertubation_Kaiser_Bunbury_2009_nodes[which(Temporal_Pertubation_Kaiser_Bunbury_2009_nodes$node_id == node_to_id & Temporal_Pertubation_Kaiser_Bunbury_2009_nodes$attribute == 'node_name'),]$value
}



write_csv(Temporal_Pertubation_Kaiser_Bunbury_2009_inter, "74_Temporal_Pertubation_Kaiser-Bunbury_2009/interactions.csv")
##DONE- LOAD INTO PACKAGE





load_emln(74)

#60 doesn't have a weight category - I need to add one - it's 1 for each interaction
Spatial_Temporal_Kolpelke_2017 <- read.csv("60_Spatial_Temporal_Kolpelke_2017/interactions.csv")
Spatial_Temporal_Kolpelke_2017_layers <- read.csv('60_Spatial_Temporal_Kolpelke_2017/layers.csv')
Spatial_Temporal_Kolpelke_2017 <- rbind(Spatial_Temporal_Kolpelke_2017, data.frame(interaction_id = 1:max(Spatial_Temporal_Kolpelke_2017$interaction_id), attribute = 'weight', value="1"))
Spatial_Temporal_Kolpelke_2017 %<>%
  group_by(interaction_id) %>%
  arrange(interaction_id)

write_csv(Spatial_Temporal_Kolpelke_2017,'./60_Spatial_Temporal_Kolpelke_2017/interactions.csv')


load_emln(60)




##fix 45 - there are dots that exist in the interactions node names that dont exist in the nodes node names
nodes45 <- read_csv('45_Spatial_Montero_2005/interactions.csv')
nodes45$value <- gsub(pattern = '\\.',replacement = '_',x = nodes45$value)


write_csv(nodes45,'45_Spatial_Montero_2005/interactions.csv')
