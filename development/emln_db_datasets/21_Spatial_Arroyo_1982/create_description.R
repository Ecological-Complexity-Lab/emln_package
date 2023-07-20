library(tidyverse)
library(bipartite)
source('~/GitHub/emln_db/noa_functions.R')

set_file_d()

write_csv(create_description_csv(description_of_article = NA,source_of_data = c('Web_of_Life','Interaction_Web_DataBase'),data_url = c('http://www.web-of-life.es/map.php','http://www.ecologia.ib.usp.br/iwdb/html/arroyo_1982.html')),'description.csv')

