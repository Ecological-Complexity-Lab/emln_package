library(tidyverse)
library(bipartite)
source('~/GitHub/emln_db/noa_functions.R')

set_file_d()

write_csv(create_description_csv(description_of_article = NA,source_of_data = 'Web_of_Life',data_url = 'http://www.web-of-life.es/map.php'),'description.csv')

