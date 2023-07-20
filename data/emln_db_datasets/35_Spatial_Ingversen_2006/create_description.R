library(tidyverse)
library(bipartite)
library(roadoi)
source('~/GitHub/emln_db/noa_functions.R')

set_file_d()
ref <- read_csv('references.csv')
info <- roadoi::oadoi_fetch(dois = ref$doi,email = 'frydmann@post.bgu.ac.il')

write_csv(create_description_csv(description_of_article = NA,
                                 source_of_data = 'Web_of_Life',data_url = 'http://www.web-of-life.es/map.php'),'description.csv')

