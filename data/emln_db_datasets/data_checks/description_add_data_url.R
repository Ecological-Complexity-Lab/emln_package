#create data url for all files
source('~/GitHub/emln_db/noa_functions.R')

description <- read_csv("description.csv")
if (T %in% grepl(filter(description, attribute == 'source'),pattern = 'Web_of_Life')) {
  description<-rbind(description, tibble(attribute = 'data_url', value = 'http://www.web-of-life.es'))
} 
if (T %in% grepl(filter(description, attribute == 'source'),pattern = 'Interaction_Web_DataBase')) {
  description<-rbind(description, tibble(attribute = 'data_url', value = 'http://www.ecologia.ib.usp.br/iwdb/index.html'))
}
if (T %in% grepl(filter(description, attribute == 'source'),pattern = 'mangal')) {
  description<-rbind(description, tibble(attribute = 'data_url', value = 'https://www.rdocumentation.org/packages/rmangal/versions/2.0.2'))
}

description <- unique(description)

write_csv(description,'description.csv')



