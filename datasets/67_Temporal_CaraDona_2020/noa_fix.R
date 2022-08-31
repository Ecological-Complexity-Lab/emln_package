##noa fix##
source('~/Github/emln_db/noa_functions.R')
set_file_d()
nodes <- read_csv('nodes.csv')
nodes <- nodes %>%
  mutate(attribute1 = 'family') %>%
  mutate(attribute2 = 'group')


##noa fix##

write_csv(nodes,'nodes.csv')
