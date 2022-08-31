library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#reference
reference1 <- tibble(doi=NA,author='Diego P. Vazquez',year='01/05/2002', paper_url = 'https://trace.tennessee.edu',
                     data_url = 'http://www.ecologia.ib.usp.br/iwdb/html/vazquez_2002.html')
reference2 <- tibble(doi='10.1046/j.1461-0248.2003.00534.x',author='Diego P. Vazquez',year='30/10/2003', paper_url = 'https://onlinelibrary.wiley.com/doi/10.1046/j.1461-0248.2003.00534.x',
                     data_url = 'http://www.ecologia.ib.usp.br/iwdb/html/vazquez_2002.html')
reference3 <- tibble(doi='10.1086/339991',author='Diego P. Vazquez',year='01/07/2002', paper_url = 'https://www.researchgate.net/publication/23174889_Ecological_Specialization_and_Susceptibility_to_Disturbance_Conjectures_and_Refutations',
                     data_url = 'http://www.ecologia.ib.usp.br/iwdb/html/vazquez_2002.html')

references <- rbind(reference1,reference2,reference3)

write_csv(references, 'references.csv')

#layers
sites <- c('Llao Llao', 'Cerro Lopez', 'Safariland', 'Arroyo Goye', 'Mascardi (nc)', 'Mascardi (c)', 'Quetrihue (nc)', 'Quetrihue (c)')
grazed <- c(F,T,F,T,F,T,F,T)
layers <- NULL
for (i in 1:length(sites)){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=sites[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value=c('space','perturbation')))
  layers <- rbind(layers,tibble(layer=i,attribute='perturbed',value=grazed[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='location', value ='Nothofagus dombeyi forest, Nahuel Huapi National Park, Argentina'))
  layers <- rbind(layers,tibble(layer=i,attribute='perturbation_type',value='moderate levels of grazing by cattle for extended periods (>20 years)'))
  
}

write_csv(layers,'layers.csv')


#nodes
plants <- read_excel("vazquez_2002.xls", sheet = "Plant sp", na = ('-'))

pollinators <- read_excel("vazquez_2002.xls", sheet = "Pollinator sp", na = ('—'))


plants <- plants %>% add_column(order= NA, .before = 2) %>%
  add_column('catalog number' = NA)

pollinators <- pollinators %>% add_column('common name' = NA, .before = 5) %>%
  add_column('life form' = NA, .before = 5) %>% add_column('origin' = NA, .before = 5)

plants$`Species no.` <- paste(plants$`Species no.`, 'pla', sep = '_')

pollinators$`Species no.` <- paste(pollinators$`Species no.`, 'pol', sep = '_')

colnames(pollinators) <- c('species no.','order','family', 'species name', 'common name', 'life form', 'origin' ,'catalog no.')
colnames(plants) <- c('species no.','order','family', 'species name', 'common name', 'life form', 'origin' ,'catalog no.')

all_nodes <- rbind(plants,pollinators)

all_nodes$`species name` <- str_replace_all(all_nodes$`species name`, ' ', '_')
all_nodes$`life form` <- str_replace_all(all_nodes$`life form`, ' ', '_')


nodes <- tibble(node_id=1:length(all_nodes$`species no.`), node_name=all_nodes$`species no.`, type=c(rep.int('plant', length(plants$`species no.`)),rep.int('pollinator',length(pollinators$`species no.`))),
                attribute1 = 'order', value1=all_nodes$order, attribute2= 'family', value2=all_nodes$family,attribute3='species name', value3=all_nodes$'species name',
                attribute4='common name' ,value4=all_nodes$'common name', attribute5 = 'life form', value5 = all_nodes$`life form`, attribute6 = 'origin', value6 = all_nodes$origin,
                attribute7 = 'catalog no.', value7 = all_nodes$`catalog no.`)


nodes <- NULL
for (i in 1:nrow(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes$`species no.`[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= c(rep.int('plant',nrow(plants)),rep.int('pollinator',nrow(pollinators)))[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='order',value=all_nodes$order[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='family',value=all_nodes$family[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='species',value=all_nodes$`species name`[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='common_name',value=all_nodes$`common name`[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='life_form',value=all_nodes$`life form`[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='origin',value=all_nodes$origin[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='catalog_no',value=all_nodes$`catalog no.`[i]))
  
}

write_csv(nodes,'nodes.csv')

#interactions
inter_table <- function (file_name, layer) {
  countryx <- read_excel(file_name)
  countryx <- countryx[-nrow(countryx),-length(countryx[1,])]
  # countryx$'Pol\Pla' <- paste(countryx$'Pol\Pla', rep.int('pol', length(countryx$'Pol\Pla')))
  # countryx[,2] <- paste(countryx[,2], 'pla')
  countryx <- column_to_rownames(countryx, var = 1)
  rownames(countryx) <-paste(rownames(countryx), 'pol', sep = '_')
  colnames(countryx) <- paste(colnames(countryx),'pla', sep = '_')
  interaction_yearx <- matrix_to_list_bipartite(countryx,group_names = c('Pollinators','Plants'))$edge_list
  interaction_yearx <- tibble(node_from = interaction_yearx$from, layer_from = layer, node_to = interaction_yearx$to, 
                              layer_to = layer, weight = interaction_yearx$weight,type ='pollination', method = 'observation')
}


llao_llao_inter <- inter_table('Llao Llao.xlsx', 'llao_llao')
cerro_lopez_inter <- inter_table('cerro lopez.xlsx', 'cerro_lopez')
safariland_inter <- inter_table('safariland.xlsx', 'safariland')
arroyo_goye_inter <- inter_table('arroyo goye.xlsx', 'arroyo_goye')
mascardi_nc_inter <- inter_table('mascardi nc.xlsx', 'mascardi_nc')
mascardi_c_inter <- inter_table('mascardi c.xlsx', 'mascardi_c')
quetruhue_nc_inter <- inter_table('quetruhue nc.xlsx', 'quetrihue_nc')
quetruhue_c_inter <- inter_table('quetrihue c.xlsx', 'quetrihue_c')

interactions_test <- rbind(llao_llao_inter,cerro_lopez_inter,safariland_inter,arroyo_goye_inter,mascardi_nc_inter,mascardi_c_inter,quetruhue_nc_inter,quetruhue_c_inter)

interactions <- inter_long(interactions_test)

write_csv(interactions, 'interactions.csv')


#description
write_csv(create_description_csv(description_of_article = 'This paper analyzes the relationship between the degree of specialization of plant and pollinator species and their response to disturbance caused by introduced cattle. The study was conducted in four grazed and four ungrazed sites in and around Nahuel Huapi National Park and surrounding areas in Rio Negro, Argentina from September 1999 to Feburary 2000. For each site, the plant-pollinator interaction network was described.' 
                                 , source_of_data = 'Interaction_Web_DataBase',data_url = references$data_url),'description.csv')




###fix###
layers <- read_csv('layers.csv')
interactions <- read_csv('interactions.csv')

layers[layers$attribute == 'name',]$value %in% unique(c(interactions[interactions$attribute == 'layer_from',]$value,interactions[interactions$attribute == 'layer_to',]$value))


layers[layers$attribute == 'name',]$value <- gsub(tolower(layers[layers$attribute == 'name',]$value),pattern = ' ',replacement = '_')
layers[layers$attribute == 'name',]$value <- gsub(layers[layers$attribute == 'name',]$value,pattern = ')$|\\(',replacement = '')

write_csv(layers,'layers.csv')
