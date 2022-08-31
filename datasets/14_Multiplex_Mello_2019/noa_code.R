library(tidyverse)
library(bipartite)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()


#reference
reference <- tibble(doi='10.1038/s41559-019-1002-3',author='Marco A. R. Mello',year='14/10/2019',
                    paper_url = 'https://www.nature.com/articles/s41559-019-1002-3', data_url = 'http://www.web-of-life.es' )
write_csv(reference, 'references.csv')

#layers
##are they spatial or interaction based?
partitions <- read_delim('partitions.txt')
layer_names <- c('frugivory','nectarivory')

layers <- NULL
for (i in 1:length(layer_names)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=(layer_names[i])))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value='interaction'))
}


write.csv(layers, file = 'layers.csv',row.names = FALSE)

#nodes
plants <- read_excel("Supplementary Data 1.xlsx",sheet = "species")[,1:2]
plants <- drop_na(plants)
bats <- read_excel("Supplementary Data 1.xlsx",sheet = "species")[4:5]
bats <- drop_na(bats)
colnames(plants) <- c('latin name', 'id')
colnames(bats) <- c('latin name', 'id')

bats <- unique(bats)
plants <- unique(plants)

bats$`latin name` <- str_replace_all(bats$`latin name`,' ','_')
plants$`latin name` <- str_replace_all(plants$`latin name`, ' ','_')

#organizing the species code tibble
species_code <- rbind(plants,bats)

#merging the species id with its latin name
nodesc1 <- read_csv("nodesc1.csv")
all_nodes <- left_join(nodesc1, species_code, by= c('id'))
aa <- left_join(species_code,nodesc1, by= c('id'))


partitions <- read_delim('partitions.txt')
colnames(partitions) <- c('id','modules','layers')

all_nodes <- left_join(all_nodes,partitions, by= c('id'))
# nodesc1 <- nodesc1[!duplicated(nodesc1$id), ]         ##there are organisms with the same species code, 
                                                              #do i need to delete the duplicates or keep them?

#creating nodes csv file
nodes <- tibble(node_id=1:length(all_nodes$id), node_name=all_nodes$id, type=c(rep.int('pollinator',74),rep.int('plant',445)),attribute1 = 'latin name', value1 =all_nodes$'latin name',
                      attribute2= 'taxon',value2=all_nodes$taxon, attribute3= 'taxon lable', value3 =all_nodes$taxon.label,
                          attribute4 = 'species size', value4 =all_nodes$species.size, attribute5= 'modules',value5= all_nodes$modules)

write_csv(nodes,'nodes.csv')


#interactions
linksc1 <- read_csv("linksc1.csv")
interactions <- tibble( node_from= linksc1$from, layer_from= linksc1$type, node_to=linksc1$to,layer_to=linksc1$type,weight=linksc1$weight, type=rep.int('seed-dispersal', nrow(linksc1)))
write_csv(interactions,'interactions.csv')

#description
write_csv(create_description_csv(description_of_article = 'In this article, they shed light on the assembly rules of a multilayer network formed by frugivory and nectarivory interactions between bats and plants in the Neotropics.', source_of_data = 'Web_of_Life', data_url = reference$data_url), 'description.csv')

