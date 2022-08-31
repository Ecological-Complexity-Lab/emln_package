library(tidyverse)
library(bipartite)
library(roadoi)
source('~/GitHub/emln_db/noa_functions.R')

set_file_d()
ref <- read_csv('references.csv')
info <- roadoi::oadoi_fetch(dois = ref$doi,email = 'frydmann@post.bgu.ac.il')

write_csv(create_description_csv(description_of_article = NA,
                                 source_of_data = c('Web_of_Life','Dryad'),data_url = c('http://www.web-of-life.es/map.php','https://datadryad.org/stash/dataset/doi:10.5061/dryad.jf3tj')),'description.csv')

