library(tidyverse)
library(bipartite)
source('~/GitHub/emln_db/noa_functions.R')

set_file_d()

write_csv(create_description_csv(description_of_article = NA,source_of_data = 'Figshare',data_url = 'https://doi.org/10.6084/m9.figshare.c.5208546.v1'),'description.csv')

