library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
reference1 <- tibble(doi='10.1111/j.1461-0248.2009.01437.x',author='Christopher N. Kaiser-Bunbury',year='15/03/2010', paper_url = 'https://onlinelibrary.wiley.com/doi/10.1111/j.1461-0248.2009.01437.x',
                     data_url = c('http://www.ecologia.ib.usp.br/iwdb/html/kaiser-bunbury_2009.html','http://www.web-of-life.es/map.php'))
reference2 <- tibble(doi='10.1016/j.ppees.2009.04.001',author='Christopher N. Kaiser-Bunbury',year='20/11/2009', paper_url = 'https://www.sciencedirect.com/science/article/pii/S1433831909000183?via%3Dihub',
                     data_url = c('http://www.ecologia.ib.usp.br/iwdb/html/kaiser-bunbury_2009.html','http://www.web-of-life.es/map.php'))

references <- rbind(reference1,reference2)

write_csv(references, 'references.csv')

#layers
layer_info <- read_excel("Mauritius 03_KaiserBunbury.xls", 
                         sheet = "matrix", range = "A1:C806")
layer_info <- distinct(layer_info, .keep_all = T)
sites <- layer_info$NetworkID %>% unique()

pertrubed <- c()
for (i in 1:nrow(layer_info)) {
  if (layer_info$Site[i] == 'CMA') {
    pertrubed <- append(x = pertrubed, values = F)
  }
  else {
    pertrubed <- append(x = pertrubed, values = T)
  }
}
layers <- NULL
for (i in 1:length(sites)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=sites[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value=c('time','perturbation')))
  layers <- rbind(layers,tibble(layer=i,attribute='perturbed',value=pertrubed[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='period',value=layer_info$Period[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='location', value ='Black River Gorges National Park, Mauritius; Collected at Le Pétrin CMA and an adjacent unrestored (nonmanaged) site (Control)'))
  layers <- rbind(layers,tibble(layer=i,attribute='perturbation_type',value='invasion of exotic plants'))
  layers <- rbind(layers,tibble(layer=i,attribute='sample_dates',value='Between 15th September 2003 and 15th March 2004, 2-weeks networks'))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value=-20.7))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value=57.733333))
}

write_csv(layers,'layers.csv')


#nodes
##restored
pollinator_restored <- read_excel("kaiser-bunbury_2009.xls", 
                         sheet = "Restored_visitation rate", skip = 6)[,1:4]
pollinator_restored$`Animal species` <- str_replace_all(pollinator_restored$`Animal species`, ' ', '_')

addition <- tibble(flower_color = rep.int(NA,length(pollinator_restored$`Animal species`)), flower_category = rep.int(NA,length(pollinator_restored$`Animal species`)),
                   floral_abundance = rep.int(NA,length(pollinator_restored$`Animal species`)))
pollinator_restored <- cbind(pollinator_restored,addition)

colnames(pollinator_restored) <- c('species', 'family', 'origin', '#_probed_flowers', 'flower_color', 'flower_category', 'floral_abundance')

plant_restored <- read_excel("kaiser-bunbury_2009.xls", 
                    sheet = "Restored_visitation rate", col_names = FALSE, 
                    col_types = c("skip", "skip", "skip", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text"), n_max = 6)

plant_restored$...1 <- paste(plant_restored$...1, plant_restored$...2)
plant_restored$...1 <- plant_restored$...1 %>% str_replace_all(' NA', '') %>% str_replace_all('NA ', '')
plant_restored <- subset(plant_restored, select = -c(2))
plant_restored <- tibble(species = t(plant_restored[1,2:ncol(plant_restored)]), family = t(plant_restored[2,2:ncol(plant_restored)]), origin = t(plant_restored[3,2:ncol(plant_restored)]), flower_color = t(plant_restored[4,2:ncol(plant_restored)]),
                flower_category = t(plant_restored[5,2:ncol(plant_restored)]), floral_abundance = t(plant_restored[6,2:ncol(plant_restored)]))
plant_restored$species <- plant_restored$species %>% str_replace_all(' ', '_')
plant_restored <- add_column(plant_restored, .before = 4, '#_probed_flowers' = rep.int(NA, length(plant_restored$species)))

plants_pollinators_restored <-  rbind(plant_restored,pollinator_restored)

probed_restored <- c(pollinator_restored$`#_probed_flowers`)

##control
pollinator_control <- read_excel("kaiser-bunbury_2009.xls", 
           sheet = "Control_visitation rate", skip = 6)[,1:4]

pollinator_control$`Animal species` <- str_replace_all(pollinator_control$`Animal species`, ' ', '_')

pollinator_control <- cbind(pollinator_control, tibble(flower_color = rep.int(NA,length(pollinator_control$`Animal species`)), flower_category = rep.int(NA,length(pollinator_control$`Animal species`)),
                   floral_abundance = rep.int(NA,length(pollinator_control$`Animal species`))))

colnames(pollinator_control) <- c('species', 'family', 'origin', '#_probed_flowers', 'flower_color', 'flower_category', 'floral_abundance')

plant_control <- read_excel("kaiser-bunbury_2009.xls", 
                            sheet = "Control_visitation rate", col_names = FALSE, 
                            col_types = c("skip", "skip", "skip", 
                                          "text", "skip", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text"), n_max = 6)

plant_control <- tibble(species = t(plant_control[1,2:ncol(plant_control)]), family = t(plant_control[2,2:ncol(plant_control)]), origin = t(plant_control[3,2:ncol(plant_control)]),
                        flower_color = t(plant_control[4,2:ncol(plant_control)]), flower_category = t(plant_control[5,2:ncol(plant_control)]), floral_abundance = t(plant_control[6,2:ncol(plant_control)]))
plant_control$species <- plant_control$species %>% str_replace_all(' ', '_')
plant_control <- add_column(plant_control, .before = 4, '#_probed_flowers' = rep.int(NA, length(plant_control$species)))

plants <- rbind(plant_control,plant_restored) %>% unique()

pollinators <- rbind(pollinator_control,pollinator_restored) %>% unique()

#the pollinator species that have duplicates
pr <- pollinators %>% filter(duplicated(species))
pr <- subset(pollinators, duplicated(x = species))

for (i in 1:length(pr$species)) {
  a <- which(grepl(pr$species[i], pollinators$species))
  show(a)
  pollinators$'#_probed_flowers'[a[1]] <- paste0(pollinators$'#_probed_flowers'[a[1]], ',', pollinators$'#_probed_flowers'[a[2]])
}

pollinators <- distinct(pollinators, species, .keep_all = T)

print(pollinators %>% filter(duplicated(species))) #should be empty

pollinators$species <- str_replace(pollinators$species, 'Campopleginae_sp_1', replacement = 'Campopleginae_sp1')

#the plant species that have duplicates
fa <- subset(plants, duplicated(x = species))

for (i in 1:length(fa$species)) {
  a <- which(grepl(fa$species[i], plants$species))
  print(a)
  plants$'floral_abundance'[a[1]] <- paste0(plants$'floral_abundance'[a[1]], ',', plants$'floral_abundance'[a[2]])
}

plants <- distinct(plants, species, .keep_all = T)

print(plants %>% filter(duplicated(species))) #should be empty

all_nodes <- rbind(plants,pollinators) %>% distinct(species, .keep_all = T)

all_nodes <- filter(all_nodes, species != 'Diptera_sp_3')
Mauritius_03_KaiserBunbury <- read_excel("Mauritius 03_KaiserBunbury.xls", 
                                         sheet = "species")

colnames(Mauritius_03_KaiserBunbury) <- c('order','family','node_code','species','kingdom','origin')
Mauritius_03_KaiserBunbury$species <- str_replace_all(Mauritius_03_KaiserBunbury$species, ' ','_')
aa <- select(all_nodes, -c(family,origin))

aa <- left_join(aa,Mauritius_03_KaiserBunbury ,by = 'species')
aa$node_code <- aa$node_code %>% str_replace_all(' ','_')

all_nodes<-aa

nodes <- tibble(node_id=1:nrow(all_nodes), node_name=all_nodes$species, type=c(rep.int('plant', length(plants$species)),rep.int('pollinator',length(pollinators$species))),
                attribute1 = 'family', value1=all_nodes$family, attribute2= 'origin', value2=all_nodes$origin, attribute3='#_probed_flowers', value3=all_nodes$`#_probed_flowers`,
                attribute4='flower_color' ,value4=all_nodes$flower_color, attribute5 = 'flower_category', value5 = all_nodes$flower_category, attribute6 = 'floral_abundance', value6 = all_nodes$floral_abundance,
attribute7 = 'code', value7 = all_nodes$node_code, attribute8 = 'kingdom',value8 = all_nodes$kingdom)

nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes$species[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= c(rep.int('plant',nrow(plants)),rep.int('pollinator',nrow(pollinators)))[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='flower_color',value=all_nodes$flower_color[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='flower_category',value=all_nodes$flower_category[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='floral_abundance',value=all_nodes$floral_abundance[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='order',value=all_nodes$order[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='family',value=all_nodes$family[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='code',value=all_nodes$node_code[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='kingdom',value=all_nodes$kingdom[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='origin',value=all_nodes$origin[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='#_probed_flowers',value=all_nodes$`#_probed_flowers`[i]))
}

write_csv(nodes,'nodes.csv')

#state nodes
plant_control$layer_id <- NA
plant_restored$layer_id <- NA
pollinator_control$layer_id <- NA
pollinator_restored$layer_id <- NA
plant_control$node_id <- NA
plant_restored$node_id <- NA
pollinator_control$node_id <- NA
pollinator_restored$node_id <- NA

for (i in 1:nrow(plant_control)) {
  plant_control$node_id[i] <- filter(nodes, attribute == 'node_name', value == plant_control$species[i])$node_id
}
for (i in 1:nrow(plant_restored)) {
  plant_restored$node_id[i] <- filter(nodes, attribute == 'node_name', value == plant_restored$species[i])$node_id
}
for (i in 1:nrow(pollinator_control)) {
  pollinator_control$node_id[i] <- filter(nodes, attribute == 'node_name', value == pollinator_control$species[i])$node_id
}
for (i in 1:nrow(pollinator_restored)) {
  pollinator_restored$node_id[i] <- filter(nodes, attribute == 'node_name', value == pollinator_restored$species[i])$node_id
}


#interactions
matrix <- read_excel("Mauritius 03_KaiserBunbury.xls")

# CMA1 <- filter(matrix,  NetworkID == 'CMA1')[,-1:-3]
# control1 <- filter(matrix,  NetworkID == 'Control1')[,-1:-3]
# CMA2 <- filter(matrix,  NetworkID == 'CMA2')[,-1:-3]
# control2 <- filter(matrix,  NetworkID == 'Control2')[,-1:-3]
# CMA3 <- filter(matrix,  NetworkID == 'CMA3')[,-1:-3]
# control3 <- filter(matrix,  NetworkID == 'Control3')[,-1:-3]
# CMA4 <- filter(matrix, NetworkID == 'CMA4')[,-1:-3]
# control4 <- filter(matrix, NetworkID == 'Control4')[,-1:-3]
# CMA5 <- filter(matrix, NetworkID == 'CMA5')[,-1:-3]
# control5 <- filter(matrix, NetworkID == 'Control5')[,-1:-3]
# CMA6 <- filter(matrix, NetworkID == 'CMA6')[,-1:-3]
# control6 <- filter(matrix, NetworkID == 'Control6')[,-1:-3]
# CMA7 <- filter(matrix, NetworkID == 'CMA7')[,-1:-3]
# control7 <- filter(matrix, NetworkID == 'Control7')[,-1:-3]
# CMA8 <- filter(matrix, NetworkID == 'CMA8')[,-1:-3]
# control8 <- filter(matrix, NetworkID == 'Control8')[,-1:-3]
# CMA9 <- filter(matrix, NetworkID == 'CMA9')[,-1:-3]
# control9 <- filter(matrix, NetworkID == 'Control9')[,-1:-3]
# CMA10 <- filter(matrix, NetworkID == 'CMA10')[,-1:-3]
# control10 <- filter(matrix, NetworkID == 'Control10')[,-1:-3]
# CMA11 <- filter(matrix, NetworkID == 'CMA11')[,-1:-3]
# control11 <- filter(matrix, NetworkID == 'Control11')[,-1:-3]
# CMA12 <- filter(matrix, NetworkID == 'CMA12')[,-1:-3]
# control12 <- filter(matrix, NetworkID == 'Control12')[,-1:-3]

# row_names <- control12$...4
# control12 <- control12[-nrow(control12),]
# control12 <- control12[,-1]
# rownames(control12) <- row_names
# control12[is.na(control12)] <- 0
# colnames(control12) <- str_replace_all(colnames(control12),' ','_')

matrix_to_list_bipartite(control12,group_names = c('plants','pollinators'))$edge_list

weight_calculation <- tibble(attribute1 = 'weight_calculation', value1 = 'interaction frequency - a measure of the number of visits multiplied by floral abundance')

interaction_calc <- function (layer_name) {
  x <- filter(matrix, NetworkID == layer_name)[,-1:-3]
  x[is.na(x)] <- 0
  x <- x[-nrow(x),]
  row_names <- x$...4
  x <- x[,-1]
  rownames(x) <- row_names
  colnames(x) <- str_replace_all(colnames(x),' ','_')
  interactions <- matrix_to_list_bipartite(x,group_names = c('pollinators','plants'))$edge_list
  interactions <- tibble(node_from = interactions$from, layer_from = layer_name, node_to = interactions$to, 
                              layer_to = layer_name, weight = interactions$weight,type ='pollination', method = 'observation')
}

CMA1 <- interaction_calc('CMA1')
CMA2 <- interaction_calc('CMA2')
CMA3 <- interaction_calc('CMA3')
CMA4 <- interaction_calc('CMA4')
CMA5 <- interaction_calc('CMA5')
CMA6 <- interaction_calc('CMA6')
CMA7 <- interaction_calc('CMA7')
CMA8 <- interaction_calc('CMA8')
CMA9 <- interaction_calc('CMA9')
CMA10 <- interaction_calc('CMA10')
CMA11 <- interaction_calc('CMA11')
CMA12 <- interaction_calc('CMA12')
control1 <- interaction_calc('Control1')
control2 <- interaction_calc('Control2')
control3 <- interaction_calc('Control3')
control4 <- interaction_calc('Control4')
control5 <- interaction_calc('Control5')
control6 <- interaction_calc('Control6')
control7 <- interaction_calc('Control7')
control8 <- interaction_calc('Control8')
control9 <- interaction_calc('Control9')
control10 <- interaction_calc('Control10')

interactions_test <- rbind(CMA1,CMA2,CMA3,CMA4,CMA5,CMA6,CMA7,CMA8,CMA9,CMA10,control1,control2,control3,control4,control5,control6,
                      control7,control8,control9,control10)

interactions <- inter_long(interactions_test)

  
write_csv(interactions, 'interactions.csv')


#description
write_csv(create_description_csv(description_of_article = 'This paper describes the community structure of pollination webs of Mauritian heathland habitats. Visitation rates and interactions strenght are compared between areas heavily invaded by introduced plant species (control site) and areas from which non-native plants were removed (restored site).', source_of_data = c('Interaction_Web_DataBase','Web_of_Life'),data_url = references$data_url),'description.csv')


