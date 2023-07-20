library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#interactions from wide to long
interactions <- read_csv('interactions.csv')
interactions <- inter_long_att(interactions)

write_csv(interactions, 'interactions.csv')


#nodes from wide to long
nodes <- read_csv('nodes.csv')
nodes <- nodes_long_conversion(nodes)

write_csv(nodes, 'nodes.csv')

