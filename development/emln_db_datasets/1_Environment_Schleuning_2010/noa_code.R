library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
references <- tibble(doi='10.1890/09-1842.1',author='Matthias Schleuning',year='01/01/2011', paper_url = 'https://esajournals.onlinelibrary.wiley.com/doi/10.1890/09-1842.1',
                    data_url = 'http://www.ecologia.ib.usp.br/iwdb/html/schleuning-et-al-2010.html')

write_csv(references, 'references.csv')

#nodes
schleuning_et_al_2010 <- read_excel("schleuning-et-al-2010.xls", 
                                    range = "AN6:BC94", col_types = c("skip", 
                                                                      "text", "skip", "text", "skip", "skip", 
                                                                      "skip", "skip", "text", "skip", "skip", 
                                                                      "skip", "skip", "text", "skip", "skip"))
seed_spreader1 <- tibble(node_name = schleuning_et_al_2010$`English name`, scientific_name = schleuning_et_al_2010$`Scientific name...4`)


seed_spreader2 <- read_excel("name correction.xlsx", sheet = "overall", range = "A9:E97", 
                             col_types = c("skip", "text", "text", 
                                           "text", "numeric"))

colnames(seed_spreader2) <- c('node_name', 'specialization_on_fruits', 'visitor/forest_species', 'no_of_visits')
seed_spreader <- left_join(seed_spreader1, seed_spreader2, "node_name")


plants1 <- tibble(node_name = schleuning_et_al_2010$Abbreviation, scientific_name = schleuning_et_al_2010$`Scientific name...2`) %>% drop_na()
plants2 <- read_excel("name correction.xlsx", range = "B4:AL8", col_types = c("text", "skip", "skip", "skip", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text"))


plants2 <- tibble(node_name = t(plants2[1,2:ncol(plants2)]), forest_type = t(plants2[2,2:ncol(plants2)]),
                  forest_stratum = t(plants2[3,2:ncol(plants2)]), plant_height = t(plants2[4,2:ncol(plants2)]))

plants <- left_join(plants1, plants2, 'node_name')
typeof(plants$plant_height)
xx <- c('specialization_on_fruits', 'visitor/forest_species', 'no_of_visits')
plants <- cbind(plants, setNames( lapply(xx, function(x) x=NA), xx) )

yy <- c('forest_type', 'forest_stratum', 'plant_height')
seed_spreader <- cbind(seed_spreader, setNames( lapply(yy, function(x) x=NA), yy) )

plants$forest_stratum <- plants$forest_stratum %>% str_replace_all('U', 'understory') %>%
  str_replace_all('M', 'midstory') %>% str_replace_all('C', 'canopy')

plants$forest_type <- plants$forest_type %>% str_replace_all('Prim', 'primary') %>% 
  str_replace_all('Sec', 'secondary')

seed_spreader$specialization_on_fruits <- seed_spreader$specialization_on_fruits %>% str_replace_all('Ob', 'obligate') %>% 
  str_replace_all('Pt', 'partial') %>% 
  str_replace_all('Op', 'opportunistic')


all_nodes <- rbind(plants, seed_spreader)
all_nodes$node_name <- all_nodes$node_name %>% str_replace_all(' ', '_') %>% 
  str_replace_all('-', '_')
all_nodes$scientific_name <- str_replace_all(all_nodes$scientific_name, ' ', '_')

all_nodes$plant_height <- as.numeric(all_nodes$plant_height)

nodes <- tibble(node_id=1:nrow(all_nodes), node_name=all_nodes$'node_name', type=c(rep.int('plant', length(plants$'node_name')),rep.int('frugivore',length(seed_spreader$'node_name'))),
                attribute1 = 'scientific_name', value1=all_nodes$'scientific_name', attribute2= 'forest_stratum', value2=all_nodes$'forest_stratum', attribute3='plant_height', value3=all_nodes$'plant_height',
                attribute4='forest_type' ,value4=all_nodes$'forest_type', attribute5 = 'visitor/forest_species', value5 = all_nodes$'visitor/forest_species', 
                attribute6 = 'no_of_visits', value6 = all_nodes$'no_of_visits', attribute7 = 'specialization_on_fruits', value7 = all_nodes$'specialization_on_fruits')

write_csv(nodes, 'nodes.csv')

#interactions
canopy <- read_excel("schleuning-et-al-2010.xls", 
                     sheet = "canopy")
midstory <- read_excel("schleuning-et-al-2010.xls", 
                       sheet = "midstory")
understory <- read_excel("schleuning-et-al-2010.xls", 
                         sheet = "understory")
primary <- read_excel("schleuning-et-al-2010.xls", 
                      sheet = "primary")
secondary <- read_excel("schleuning-et-al-2010.xls", 
           sheet = "secondary")

name_correction <- read_excel("name correction.xlsx", 
                              sheet = "Sheet1", col_names = FALSE)

# #gives me the shorthand writing for the names available in the matrix
# aa <- filter(name_correction, name_correction$...2 %in% canopy$...1) 
# #sort the two dataframes
# canopy <- sort(canopy)
# aa <- aa[order(aa$...2),]
# #create formatted species names
# aa$...1 <- aa$...1 %>% str_replace_all(' ', '_') %>% str_replace_all('-','_')
# #set rows
# rownames(canopy) <- aa$...1

ordered_english_names <- function (layer) {
#gives me the shorthand writing for the names available in the matrix
aa <- filter(name_correction, name_correction$...2 %in% layer$...1) 
#sort the two dataframes
layer <- sort(layer)
aa <- aa[order(aa$...2),]
#create formatted species names
aa$...1 <- aa$...1 %>% str_replace_all(' ', '_') %>% str_replace_all('-','_')
colnames(layer) <- colnames(layer) %>% str_replace_all(' ','_')
#delete irrelevent column from layer
layer = subset(layer, select = -1 )
#set rows
rownames(layer) <- aa$...1

return(layer)
}

canopy <- ordered_english_names(canopy)
midstory <- ordered_english_names(midstory)
understory <- ordered_english_names(understory)
primary <- ordered_english_names(primary)
secondary <- ordered_english_names(secondary)

interaction_table <- function (matrix, layer){
canopy_inter <- matrix_to_list_bipartite(matrix, group_names = c('frugivores','plants'))$edge_list
canopy_inter <- tibble(node_from = canopy_inter$from, layer_from = layer, node_to = canopy_inter$to, 
           layer_to = layer, weight = canopy_inter$weight,type ='frugivory')
}

canopy_inter <- interaction_table(canopy, 'canopy')
midstory_inter <- interaction_table(midstory, 'midstory')
understory_inter <- interaction_table(understory, 'understory')
primary_inter <- interaction_table(primary, 'primary')
secondary_inter <- interaction_table(secondary, 'secondary')

interactions <- rbind(canopy_inter,midstory_inter,understory_inter,primary_inter,secondary_inter)
method <- tibble(attribute1 = rep.int('method',nrow(interactions)), value1 = rep.int('observation',nrow(interactions)))
interactions <- cbind(interactions,method)

write_csv(interactions, 'Noa/interactions.csv')

#layer 
sites <- c('canopy','midstory','understory','primary','secondary')

layers <- NULL
for (i in 1:length(sites)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=sites[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value='environment'))
  layers <- rbind(layers,tibble(layer=i,attribute='locality of study', value ='Kakamega Forest, western Kenya'))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value=0.250512))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value=34.871005))
  
  
}
write_csv(layers,'layers.csv')

#description
write_csv(create_description_csv(description_of_article = NA,source_of_data = 'Interaction_Web_DataBase',data_url =  references$data_url), 'description.csv')
