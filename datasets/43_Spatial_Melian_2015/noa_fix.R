##noa fix##
source('~/Github/emln_db/noa_functions.R')
set_file_d()
nodes <- read_csv('nodes.csv')
nodes <- nodes %>%
  mutate(attribute2 = 'family')
##noa fix##

write_csv(nodes,'nodes.csv')
