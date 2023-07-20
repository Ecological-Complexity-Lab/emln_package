##noa fix##
source('~/Github/emln_db/noa_functions.R')
set_file_d()
nodes <- read_csv('nodes.csv')
nodes <- nodes %>%
  mutate(attribute1 = 'sample_size', value1 = c(nodes$value1[1:61],rep.int(NA,250))) %>%
  mutate(attribute2 = 'species') %>%
  mutate(attribute3 = 'genus', value3 = c(rep.int(NA,61),nodes$value1[62:311]))
##noa fix##

write_csv(nodes,'nodes.csv')
