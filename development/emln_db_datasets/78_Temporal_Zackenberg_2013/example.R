library(tidyverse)
library(bipartite)

source('data_structure_functions.R')


###
#Zackenberg
###

d <- read_csv('EMLN_Datasets/Zackenberg/ZAC CR.csv') 

#layers template
layers <- unique(d$Date_collected)

layers_table <- tibble(layer_id=1:length(layers), layer_name=layers, type='interaction')
write_csv(layers_table, './EMLN_Datasets/Zackenberglayers_noa.csv')

#nodes template
plants <- unique(d$Plant)
pollinators <- unique(d$Pollinator)
nodes_table <- tibble(node_id=1:(length(plants)+length(pollinators)),
                      node_name=c(plants,pollinators),
                      type=c(rep('plant', length(plants)),rep('polli', length(pollinators))))
write_csv(nodes_table, './EMLN_Datasets/Zackenberg/nodes_noa.csv')

#interactions template
interactions_table <- tibble(node_from=d$Pollinator, layer_from=d$Date_collected, node_to=d$Plant, layer_to=d$Date_collected, weight=1, type='interaction')
write_csv(interactions_table, './EMLN_Datasets/Zackenberg/interactions_noa.csv')

#referance template 
ref_zan <- tibble(doi='10.1371/journal.pone.0081694',author='Rasmussen C.', year=2013,paper_url=, data_url=)

###
#Trojelsgaard2015
###


#layers template
sites <- read_csv("EMLN_Datasets/Trojelsgaard2015/Sites.csv")
layers_Trojelsgaard2015 <- tibble(layer_id=1:length(sites$sites), layer_name=sites$sites, description= 'measured by latitude(attr1) and longtitude(attr2) - 14 different sites',type = 'space' ,directed=F, attribute_1=sites$latitude, attribute_2= sites$longitude)
write_csv(layers_Trojelsgaard2015, './EMLN_Datasets/Trojelsgaard2015/layers_noa.csv')

#nodes template
pollinators_unique <- unique(all_sites$node_to)
plants_unique <- unique(all_sites$node_from)
node_all_sites <- tibble(node_id=1:(length(pollinators_unique)+length(plants_unique)), node_name=c(pollinators_unique,plants_unique),type='interaction')
write_csv(node_all_sites, './EMLN_Datasets/Trojelsgaard2015/nodes_noa.csv')

#interactions template
site_table <- function (site, layer) {
  sitex <- read_csv(paste0('EMLN_Datasets/Trojelsgaard2015/',site,'.csv'))
  site_x_names <- sitex$X1 
  sitex <- sitex[,-1]
  rownames(sitex) <- site_x_names
  interactions_sitex <- matrix_to_list_bipartite(sitex,group_names = c('Pollinator','Plants'))$edge_list
  interactions_sitex <- tibble(node_from=interactions_sitex$from, layer_from= layer, node_to=interactions_sitex$to, 
                               layer_to = layer, weight=interactions_sitex$weight,type='interaction')
  
}

site_1 <- site_table('Site1_WesternSahara1', 'Tenerife Teno site 1')

site_2 <- site_table('Site2_WesternSahara2', 'Tenerife Teno site 2')

site_3 <- site_table('Site3_Fuerteventura1', 'Tenerife South site 2')

site_4 <- site_table('Site4_Fuerteventura2', 'Tenerife South site 1')

site_5 <- site_table('Site5_GranCanaria1', 'Gran Canaria site 2')

site_6 <- site_table('Site6_GranCanaria2', 'Gran Canaria site 1')

site_7 <- site_table('Site7_TenerifeSouth1', 'Fuerteventura site 2')

site_8 <- site_table('Site8_TenerifeSouth2', 'Fuerteventura site 1')

site_9 <- site_table('Site9_TenerifeTeno1', 'Western Sahara site 2')

site_10 <- site_table('Site10_TenerifeTeno2', 'Western Sahara site 1')

site_11 <- site_table('Site11_Gomera1', 'El Hierro site 2')

site_12 <- site_table('Site12_Gomera2', 'El Hierro site 1')

site_13 <- site_table('Site13_Hierro1', 'La Gomera site 2')

site_14 <- site_table('Site14_Hierro2', 'La Gomera site 1')

all_sites <- rbind(site_1,site_2,site_3,site_4,site_5,site_6,site_7,site_8,site_9,site_10,site_11,site_12,site_13,site_13,site_14)

write_csv(all_sites, './EMLN_Datasets/Trojelsgaard2015/interactions_noa.csv')


  
  
  
  
  