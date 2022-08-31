library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()


##layers
layers <- NULL
for (i in 1:4){
  layers <- rbind(layers,tibble(layer=i,attribute='name',value=(c(2007,2008,2009,2010))[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value='time'))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value=38.897942))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value=-104.872269))
  layers <- rbind(layers,tibble(layer=i,attribute='location',value='Shangri-La Alpine Botanical Garden, Yunnan Province, China'))
}

write_csv(layers, 'layers.csv')

##nodes
                                      ##2007##
X2007 <- read_excel("2007.xlsx", col_names = FALSE)
#list of id and species
need_id_7 <- X2007
need_id_7$'...2' <- paste0(need_id_7[['...2']],'_',need_id_7[['...3']])
need_id_7 = subset(need_id_7, select = -c(3) )
need_id_7[1:3,2] <- NA
need_id_7[2,3:ncol(need_id_7)] <- as.list(paste0(need_id_7[2,3:ncol(need_id_7)],'_',need_id_7[3,3:ncol(need_id_7)]))

#creating ordred matrix
X2007 <- X2007[-2:-3,]
X2007 <- X2007[,-2:-3]
pollinators_2007 <- X2007[1,]
pollinators_2007 <- paste0(pollinators_2007, '_polli')
pollinators_2007<-pollinators_2007[-1]
plants_2007 <- X2007$...1
plants_2007<-plants_2007[!sapply(plants_2007,is.na)]
plants_2007 <- paste0(plants_2007,'_pla')
X2007 <- X2007[-1,-1]
colnames(X2007) <- pollinators_2007
rownames(X2007) <- plants_2007


                                    ##2008##
X2008 <- read_excel("2008.xlsx", col_names = FALSE)
#list of id and species
need_id_8 <- X2008
need_id_8$'...2' <- paste0(need_id_8[['...2']],'_',need_id_8[['...3']])
need_id_8 = subset(need_id_8, select = -c(3,4) )
need_id_8[1:3,2] <- NA
need_id_8[2,3:ncol(need_id_8)] <- as.list(paste0(need_id_8[2,3:ncol(need_id_8)],'_',need_id_8[3,3:ncol(need_id_8)]))

#creating ordered matrix
X2008 <- X2008[-2:-3,]
X2008 <- X2008[,-2:-4]
pollinators_2008 <- X2008[1,]
pollinators_2008 <- paste0(pollinators_2008, '_polli')
pollinators_2008<-pollinators_2008[-1]
plants_2008 <- X2008$...1
plants_2008<-plants_2008[!sapply(plants_2008,is.na)]
plants_2008 <- paste0(plants_2008,'_pla')
X2008 <- X2008[-1,-1]
colnames(X2008) <- pollinators_2008
rownames(X2008) <- plants_2008

                                ##2009##
X2009 <- read_excel("2009.xlsx", col_names = FALSE)
#list of id and species
need_id_9 <- X2009
need_id_9$'...2' <- paste0(need_id_9[['...2']],'_',need_id_9[['...3']])
need_id_9 = subset(need_id_9, select = -c(3,4) )
need_id_9[1:3,2] <- NA
need_id_9[2,3:ncol(need_id_9)] <- as.list(paste0(need_id_9[2,3:ncol(need_id_9)],'_',need_id_9[3,3:ncol(need_id_9)]))

#creating ordred matrix
X2009 <- X2009[-2:-3,]
X2009 <- X2009[,-2:-4]
pollinators_2009 <- X2009[1,]
pollinators_2009 <- paste0(pollinators_2009, '_polli')
pollinators_2009<-pollinators_2009[-1]
plants_2009 <- X2009$...1
plants_2009<-plants_2009[!sapply(plants_2009,is.na)]
plants_2009 <- paste0(plants_2009,'_pla')
X2009 <- X2009[-1,-1]
colnames(X2009) <- pollinators_2009
rownames(X2009) <- plants_2009

                                  ##2010##
X2010 <- read_excel("2010.xlsx", col_names = FALSE)[-1,]
#list of id and species
need_id_10 <- X2010
need_id_10$'...2' <- paste0(need_id_10[['...2']],'_',need_id_10[['...3']])
need_id_10 = subset(need_id_10, select = -c(3,4) )
need_id_10[1:3,2] <- NA
need_id_10[2,3:ncol(need_id_10)] <- as.list(paste0(need_id_10[2,3:ncol(need_id_10)],'_',need_id_10[3,3:ncol(need_id_10)]))

#creating ordred matrix
X2010 <- X2010[-2:-3,]
X2010 <- X2010[,-2:-4]
pollinators_2010 <- X2010[1,]
pollinators_2010 <- paste0(pollinators_2010, '_polli')
pollinators_2010<-pollinators_2010[-1]
plants_2009 <- X2010$...1
plants_2009<-plants_2009[!sapply(plants_2009,is.na)]
plants_2009 <- paste0(plants_2009,'_pla')
X2010 <- X2010[-1,-1]
colnames(X2010) <- pollinators_2010
rownames(X2010) <- plants_2009



#interactions
inter_table <- function (year, layer) {
  interaction_yearx <- matrix_to_list_bipartite(year,group_names = c('Pollinator','Plants'))$edge_list
  interactions <- tibble(node_from = interaction_yearx$from, layer_from = layer, node_to= interaction_yearx$to, layer_to = layer, weight = interaction_yearx$weight, 
                         method = 'observation')
  return(interactions)
}
  # interaction_yearx <- tibble(node_from=interaction_yearx$from, layer_from= layer, node_to=interaction_yearx$to, 
  #                             layer_to = layer, weight=interaction_yearx$weight,type='pollination')}

inter7 <- inter_table(X2007,'2007')
inter8 <- inter_table(X2008,'2008')
inter9 <- inter_table(X2009,'2009')
inter10 <- inter_table(X2010,'2010')

interactions_test <- rbind(inter7,inter8,inter9,inter10)

interactions <- NULL
for (i in 1:nrow(interactions_test)){
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='node_from',value=interactions_test$node_from[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='layer_from',value=interactions_test$layer_from[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='node_to',value=interactions_test$node_to[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='layer_to',value=interactions_test$layer_to[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='weight',value=interactions_test$weight[i]))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='type',value=rep('pollination')))
  interactions <- rbind(interactions,tibble(interaction_id=i,attribute='method',value='observation'))
  
}

#check
test <- unique(interactions$interaction_id)

write_csv(interactions, 'interactions.csv')


#nodes

plant_id07 <- need_id_7[4:nrow(need_id_7),1:2]
plant_id08 <- need_id_8[4:nrow(need_id_8),1:2]
plant_id09 <- need_id_9[4:nrow(need_id_9),1:2]
plant_id10 <- need_id_10[4:nrow(need_id_10),1:2]

plant_id <- bind_rows(plant_id07,plant_id08,plant_id09,plant_id10)

plant_id <- unique(plant_id)

plant_id$...2 <- str_replace_all(plant_id$...2, pattern = '_NA',replacement = '')

plant_id$...1 <- paste0(plant_id$...1,'_pla')

pollinator_id07 <- need_id_7[1:2,3:ncol(need_id_7)]
pollinator_id08 <- need_id_8[1:2,4:ncol(need_id_8)]
pollinator_id09 <- need_id_9[1:2,4:ncol(need_id_9)]
pollinator_id10 <- need_id_10[1:2,4:ncol(need_id_10)]

pollinator_id <- cbind(pollinator_id07,pollinator_id08,pollinator_id09,pollinator_id10)

pollinator_id <- tibble(polli_id = t(pollinator_id[1,]), polli_name=t(pollinator_id[2,]))

pollinator_id <- pollinator_id %>% unique()

pollinator_id$polli_name <- str_replace_all(pollinator_id$polli_name, pattern = '_NA',replacement = '')

pollinator_id$polli_id <- paste0(pollinator_id$polli_id,'_polli')



nodes <- tibble(node_id=1:(nrow(plant_id)+nrow(pollinator_id)),node_name=c(plant_id$...1,pollinator_id$polli_id),type=c(rep.int('plant',nrow(plant_id)),rep.int('pollinator',nrow(pollinator_id))),
                attribute1='genus_species',value1= c(plant_id$...2, pollinator_id$polli_name))

nodes <- NULL
for (i in 1:(nrow(plant_id)+nrow(pollinator_id))) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=c(plant_id$...1,pollinator_id$polli_id)[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= c(rep.int('plant',nrow(plant_id)),rep.int('pollinator',nrow(pollinator_id)))[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='genus_species',value=c(plant_id$...2, pollinator_id$polli_name)[i]))

}


write_csv(nodes, 'nodes.csv')


##reference
references <- tibble(doi='10.1371/journal.pone.0032663',author='Qiang Fang',year='08/03/2012',paper_url = 'https://doi.org/10.1371/journal.pone.0032663', data_url = 'https://figshare.com/articles/dataset/Relative_Stability_of_Core_Groups_in_Pollination_Networks_in_a_Biodiversity_Hotspot_over_Four_Years/127797')

write_csv(references, 'references.csv')

#description
write_csv(create_description_csv(description_of_article = 'the stability of a pollination networks biodiverse hotspot over four years',source_of_data = 'web',data_url = references$data_url), 'description.csv')


