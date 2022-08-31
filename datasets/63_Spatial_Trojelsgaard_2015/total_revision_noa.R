library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
library('lubridate')

source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

# 
# #set working directory
# set_file_d()

#interactions from wide to long
interactions <- read_csv('interactions.csv')
write_csv(interactions, 'interactions_old.csv')
interactions <- inter_long_att(interactions)

write_csv(interactions, 'interactions.csv')


#nodes from wide to long
nodes <- read_csv('nodes.csv')
write_csv(nodes, 'nodes_old.csv')
nodes <- nodes_long_conversion(nodes)

write_csv(nodes, 'nodes.csv')

#split layer types
layers <- read_csv('layers.csv')
write_csv(layers, 'layers_old.csv')
layers <- split_type(layers)

write_csv(layers, 'layers.csv')

#references
ref <- read_csv('references.csv')
ref <- cbind(ref,tibble(paper_url = 'https://royalsocietypublishing.org/doi/full/10.1098/rspb.2014.2925', data_url = 'https://datadryad.org/stash/dataset/doi:10.5061/dryad.76173'))
ref <- mutate(ref, year = as.Date(parse_date_time('07/03/2015',"dmy")))
write_csv(ref,'references.csv')

#description
write_csv(create_description_csv(description_of_article = NA , source_of_data = 'Dryad',data_url = 'https://datadryad.org/stash/dataset/doi:10.5061/dryad.76173'), 'description.csv')
