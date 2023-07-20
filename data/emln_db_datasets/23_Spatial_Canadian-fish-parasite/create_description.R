library(tidyverse)
library(bipartite)
library(roadoi)
source('~/GitHub/emln_db/noa_functions.R')

set_file_d()
ref <- read_csv('references.csv')
info <- roadoi::oadoi_fetch(dois = ref$doi,email = 'frydmann@post.bgu.ac.il')

write_csv(create_description_csv(description_of_article = 'Data on host use by parasites were obtained from 7 Canadian freshwater systems, either large lakes or rivers, in which most fish species have been surveyed for parasites.',
                                 source_of_data = 'Interaction_Web_DataBase',data_url = 'http://www.ecologia.ib.usp.br/iwdb/html/canadian_fish.html'),'description.csv')

