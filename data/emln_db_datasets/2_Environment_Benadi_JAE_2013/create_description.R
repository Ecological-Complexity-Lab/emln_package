library(tidyverse)
library(bipartite)
source('~/GitHub/emln_db/noa_functions.R')

set_file_d()

write_csv(create_description_csv(description_of_article = NA,source_of_data = 'Dryad',data_url = 'https://datadryad.org/stash/dataset/doi:10.5061/dryad.8mn44'),'description.csv')

