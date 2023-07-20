library(tidyverse)
library(bipartite)
library(roadoi)
source('~/GitHub/emln_db/noa_functions.R')

set_file_d()
ref <- read_csv('references.csv')
info <- roadoi::oadoi_fetch(dois = ref$doi,email = 'frydmann@post.bgu.ac.il')

write_csv(create_description_csv(description_of_article = NA,
                                 source_of_data = 'figshare',data_url = 'https://figshare.com/articles/dataset/_Beta_Diversity_of_Plant_Pollinator_Networks_and_the_Spatial_Turnover_of_Pairwise_Interactions_/1234708'),'description.csv')

