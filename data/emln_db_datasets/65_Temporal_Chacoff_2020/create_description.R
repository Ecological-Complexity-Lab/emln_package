library(tidyverse)
library(bipartite)
library(roadoi)
source('~/GitHub/emln_db/noa_functions.R')

set_file_d()
ref <- read_csv('references.csv')
info <- roadoi::oadoi_fetch(dois = ref$doi,email = 'frydmann@post.bgu.ac.il')

write_csv(create_description_csv(description_of_article = NA,
                                 source_of_data = 'Dryad',data_url = 'https://datadryad.org/stash/dataset/doi:10.5061/dryad.8cz8w9gm1'),'description.csv')

