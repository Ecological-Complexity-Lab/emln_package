library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
references <- tibble(doi='10.1007/s00442-007-0946-1',author='Ignasi Bartomeus',year='10/01/2008', paper_url = 'https://doi.org/10.1007/s00442-007-0946-1',data_url = c('http://www.ecologia.ib.usp.br/iwdb/html/bartomeus_2008.html','http://www.web-of-life.es/map.php'))
write_csv(references, 'references.csv')

##nodes
Bartomeus_Ntw_nceas <- read_excel("Bartomeus_Ntw_nceas.copy.xlsx",
                                       sheet = "Ntw_raw_data")

plants <- Bartomeus_Ntw_nceas[,3:5]
insect <- Bartomeus_Ntw_nceas[,6:9]

plants <- plants %>% unique()
insect <- insect %>% unique()

colnames(plants) <- c('id','family','genus species')
colnames(insect) <- c('id','order','family', 'genus species')
plants <- plants %>% add_column('order' = NA, 'type' = 'Plant')
insect <- insect %>% add_column('type' = 'Pollinator')
plants$`genus species` <- str_replace_all(plants$`genus species`, '_',' ')
insect$`genus species` <- str_replace_all(insect$`genus species`, '_',' ')

plants$id <- paste0(plants$id,sep = '_','pla')
insect$id <- paste0(insect$id,sep = '_','pol')


all_nodes <- rbind(plants,insect)
all_nodes$`genus species` <- str_replace_all(all_nodes$`genus species`, ' ','_')
all_nodes$type <- tolower(all_nodes$type)

check <- unique(all_nodes$id)

nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes$id[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= all_nodes$type[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='genus_species',value=all_nodes$`genus species`[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='family',value=all_nodes$family[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='order',value=all_nodes$order[i]))

}

nodes$value<-nodes$value %>% tolower()

write_csv(nodes,'nodes.csv')

#layers
          ##problem with sites - SEL10P,SEL1P,SEL1OP = SAME SITE? also, FAR site is missing###
sites <- Bartomeus_Ntw_nceas$Site
sites <- sites %>% unique()

view(sites)

#checking for pertrubed sites
look.for <- c('Carpobrotus_spp._acinaciformis', 'Opuntia_stricta')
invaded_sites <- Bartomeus_Ntw_nceas[Bartomeus_Ntw_nceas$Plant_Gen_Sp %in% look.for, ]$Site
invaded_sites <- invaded_sites %>% unique()

invaded_or_not <- NULL
for (i in 1:length(sites)){
  invaded_or_not <- c(invaded_or_not,sites[i] %in% invaded_sites)
}

layers <- NULL
for (i in 1:length(sites)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=sites[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value=c('environment','perturbation')))
  layers <- rbind(layers,tibble(layer=i,attribute='location',value='Cap de Creus, Catalonia, Spain'))
  layers <- rbind(layers,tibble(layer=i,attribute='perturbed',value= invaded_or_not[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='invasive_organism',value= ifelse(grepl('CA',sites[i]), 'Carpobrotus', 'Opuntia')))
  layers <- rbind(layers,tibble(layer=i,attribute='perturbation_type',value= 'presence of invasive species'))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value= '42.3181'))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value= '3.3198'))



}

write_csv(layers,'layers.csv')

#interactions
BAT1CA <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                                       sheet = "Ntw_builder", skip = 4)

write_csv(BAT1CA, 'BAT1CA.csv')

BAT2CA <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 4)

write_csv(BAT2CA, 'BAT2CA.csv')

FRA1OP <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)

FRA1OP <- tidy_matrix('FRA1OP.csv')

write_csv(FRA1OP[-1,], 'FRA1OP.csv')

FRA2OP <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)
write_csv(FRA2OP[-1,], 'FRA2OP.csv')

MED1CA <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)
write_csv(MED1CA[-1,], 'MED1CA.csv')

MED2CA <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)
write_csv(MED2CA[-1,], 'MED2CA.csv')

MED3CA <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)
write_csv(MED3CA[-1,], 'MED3CA.csv')

MED4CA <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)
write_csv(MED4CA[-1,], 'MED4CA.csv')

MIQ1OP <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)
write_csv(MIQ1OP[-1,], 'MIQ1OP.csv')

MIQ2OP <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)
write_csv(MIQ2OP[-1,], 'MIQ2OP.csv')

SEL1OP <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)
write_csv(SEL1OP[-1,], 'SEL1OP.csv')

SEL2OP <- read_excel("Bartomeus_Ntw_nceas.copy.xls",
                     sheet = "Ntw_builder", skip = 3)
write_csv(SEL2OP[-1,], 'SEL2OP.csv')

inter_table <- function (file_name, layer) {
  countryx <- read_csv(file_name, skip = 1)
  #countryx <- countryx[-nrow(countryx),-length(countryx[1,])]
  if ('Row Labels' %in% colnames(countryx)){
  countryx <- column_to_rownames(countryx, var = 'Row Labels')
  } else {
    rownames(countryx) <- countryx[,1]
    countryx <- countryx[,-1]
  }
  View(countryx)
  interaction_yearx <- matrix_to_list_bipartite(countryx,group_names = c('Pollinators','Plants'))$edge_list
  interaction_yearx <- tibble(node_from=interaction_yearx$from, layer_from= layer, node_to=interaction_yearx$to,
                              layer_to = layer, weight=interaction_yearx$weight,type='pollination', method = 'observation')}

BAT1CA_inter <- inter_table('BAT1CA.csv', 'BAT1CA')
BAT2CA_inter <- inter_table('BAT2CA.csv', 'BAT2CA')
FRA1OP_inter <- inter_table('FRA1OP.csv', 'FRA1OP')
FRA2OP_inter <- inter_table('FRA2OP.csv', 'FRA2OP')
MED1CA_inter <- inter_table('MED1CA.csv', 'MED1CA')
MED2CA_inter <- inter_table('MED2CA.csv', 'MED2CA')
MED3CA_inter <- inter_table('MED3CA.csv', 'MED3CA')
MED4CA_inter <- inter_table('MED4CA.csv', 'MED4CA')
MIQ1OP_inter <- inter_table('MIQ1OP.csv', 'MIQ1OP')
MIQ2OP_inter <- inter_table('MIQ2OP.csv', 'MIQ2OP')
SEL1OP_inter <- inter_table('SEL1OP.csv', 'SEL1OP')
SEL2OP_inter <- inter_table('SEL2OP.csv', 'SEL2OP')

read_csv('MED4CA.csv')


interactions_test <- rbind(BAT1CA_inter,BAT2CA_inter, FRA1OP_inter, FRA2OP_inter, MED1CA_inter, MED2CA_inter,
                      MED3CA_inter, MED4CA_inter, MIQ1OP_inter, MIQ2OP_inter, SEL1OP_inter, SEL2OP_inter)

interactions_test$node_to <- str_replace_all(interactions$node_to, ' ','_')

interactions<- inter_long(interactions_test)

write_csv(interactions,'interactions.csv')

#description
write_csv(create_description_csv(description_of_article = 'The authors compared uninvaded and invaded ecological communities, to examine how two species of invasive plants with large and showy flowers (Carpobrotus affine acinaciformis and Opuntia stricta) affect the structure of Mediterranean plant–pollinator networks at the Natural Park of Cap de Creus, Catalonia.', source_of_data = c('Web_of_Life','Interaction_Web_DataBase'),data_url = references$data_url),'description.csv')




##fix##

interactions <- read_csv('interactions.csv')
nodes <- read_csv('nodes.csv')

unique(c(interactions[interactions$attribute == 'node_from',]$value,interactions[interactions$attribute == 'node_to',]$value))

nodes[nodes$attribute == 'node_name',]$value


new_interactions <- read_excel('Bartomeus_Ntw_nceas.xls',sheet = 'Ntw_raw_data')
#new_interactions <- cbind(data.frame(interaction_id = 1:nrow(new_interactions)),new_interactions)

new_interactions$Plant_id <- paste0(new_interactions$Plant_id, '_pla')
new_interactions$Insect_id <- paste0(new_interactions$Insect_id, '_pol')


new_interactions_temp <- NULL
new_interactions <- new_interactions %>%
  group_by(Site,Plant_id,Insect_id) %>%
  arrange() %>%
  summarize(Weight = sum(Number_of_visits_per6min), .groups = 'rowwise')

n <- read_excel('Bartomeus_Ntw_nceas.xls',sheet = 'Ntw_raw_data')
n <- n[,-2]
n <- n[,-1]
n <- distinct(n)
for (i in 1:nrow(new_interactions)) {

}


inter <- NULL
for (i in 1:nrow(new_interactions)) {
   inter <- rbind(inter, data.frame(interaction_id = i, attribute = 'node_from', value = paste0(new_interactions$Plant_id[[i]], '_pla')))
   inter <- rbind(inter, data.frame(interaction_id = i, attribute = 'layer_from', value = new_interactions$Site[[i]]))
   inter <- rbind(inter, data.frame(interaction_id = i, attribute = 'node_to', value = paste0(new_interactions$Insect_id[[i]], '_pol')))
   inter <- rbind(inter, data.frame(interaction_id = i, attribute = 'layer_to', value = new_interactions$Site[[i]]))
   inter <- rbind(inter, data.frame(interaction_id = i, attribute = 'type', value = 'pollination'))
   inter <- rbind(inter, data.frame(interaction_id = i, attribute = 'weight', value = new_interactions$Weight[[i]]))
}

write_csv(inter,'interactions.csv')






#noa fix
interactions <- read_csv('interactions.csv') %>% pivot_wider(id_cols = interaction_id, names_from = attribute, values_from = value)
interactions[interactions$layer_from == 'SEL10P',]$layer_to <- 'SEL1OP'
interactions[interactions$layer_from == 'SEL10P',]$layer_from <- 'SEL1OP'
interactions[interactions$layer_from == 'SEL1P',]$layer_to <- 'SEL1OP'
interactions[interactions$layer_from == 'SEL1P',]$layer_from <- 'SEL1OP'

interactions <- interactions %>%
  pivot_longer(!interaction_id, names_to = 'attribute', values_to = 'value')

write_csv(interactions, 'interactions.csv')


layers <- read_csv('layers.csv') %>% pivot_wider(id_cols = layer, names_from = attribute, values_from = value, )




#noa_fix
desc <- read_csv('description.csv')
desc[desc$attribute=='multilayer_network_type' & desc$value=='Spatial',]$value <- 'Environment'

write_csv(desc,'description.csv')

