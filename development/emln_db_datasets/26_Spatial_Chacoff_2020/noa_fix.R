##noa fix##
source('~/Github/emln_db/noa_functions.R')
set_file_d()
nodes <- read_csv('nodes.csv')
nodes <- nodes %>%
  mutate(attribute2 = 'corolla_length', value2 = c(nodes$value2[1:45],rep.int(NA,135))) %>%
  mutate(attribute3 = 'corolla_aperture', value3 = c(nodes$value3[1:45],rep.int(NA,135))) %>%
  mutate(attribute4 = 'height_mean', value4 = c(nodes$value4[1:45],rep.int(NA,135))) %>%
  mutate(attribute5 = 'flower_abundance', value5 = c(nodes$value5[1:45],rep.int(NA,135))) %>%
  mutate(attribute6 = 'body_thickness') %>%
  mutate(attribute7 = 'body_length', value7 = c(rep.int(NA,45),nodes$value2[46:180])) %>%
  mutate(attribute8 = 'proboscis_length', value8 = c(rep.int(NA,45),nodes$value3[46:180])) %>%
  mutate(attribute9 = 'proboscis_width', value9 = c(rep.int(NA,45),nodes$value4[46:180])) %>%
  mutate(attribute10 = 'body_width', value10 = c(rep.int(NA,45),nodes$value5[46:180]))
  
##noa fix##

write_csv(nodes,'nodes.csv')
