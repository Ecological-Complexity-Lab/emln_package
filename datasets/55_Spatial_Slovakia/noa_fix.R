##noa fix##
source('~/Github/emln_db/noa_functions.R')
set_file_d()
nodes <- read_csv('nodes.csv')
nodes <- nodes %>%
  mutate(attribute3 = 'groupIR') %>%
  mutate(attribute4 = 'host_type') %>%
  mutate(attribute5 = 'exclude')
  

##noa fix##

write_csv(nodes,'nodes.csv')
