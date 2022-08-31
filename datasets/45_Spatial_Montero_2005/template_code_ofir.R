library(tidyverse)
library(bipartite)

datasets<-list()
for(i in 37:38){
  name<-paste0("M_PL_0",i,".csv")
  datasets[[i-36]]<-read.csv((name))
}


#### correction (there was a missing layer) #####
datasets[[3]] <- read.csv('./web-of-life_2022-08-21_142305/M_PL_043.csv')
reference_data<-read.csv("references_data.csv")
ref_addition <- read.csv('./web-of-life_2022-08-21_142305/references.csv')

colnames(reference_data) <- gsub(colnames(ref_addition), pattern = '[[:punct:]]',replacement = '_')
colnames(ref_addition) <- gsub(colnames(ref_addition), pattern = '[[:punct:]]',replacement = '_')
  
reference_data <- rbind(reference_data,ref_addition)
#layers
layers=NULL
for(i in 1:3){
  layers<-rbind(layers,tibble(layer=i,attribute='layer_id',value=reference_data$ID[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=reference_data$Locality_of_Study[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='type_of_interaction',value=reference_data$Type_of_interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=reference_data$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=reference_data$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_species',value=reference_data$Species[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=reference_data$Interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='connectance',value=reference_data$Connectance[i]))
}
#write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Montero2005/layers.csv')
write_csv(layers, 'layers.csv')

#interactions
generate_table<-function(data,layer){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 2:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        output<-rbind(output,tibble(node_from=data$X[[i]],layer_from=layer,node_to=colnames(data)[[j]],layer_to=layer,weight=square,type="pollination"))
      }
    }
  }
  return(output)
}
interactions=NULL
for(i in 1:3) {
  interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/spatial_Montero2005/interactions.csv')


###correction of the interactions, more interactions due to an additional layer. need to turn it into a long format###
interactions <- cbind(data.frame(interaction_id = 1:401),interactions)
interactions$layer_from <- as.character(interactions$layer_from)
interactions$layer_to <- as.character(interactions$layer_to)
interactions$weight <- as.character(interactions$weight)
interactions$node_from <- gsub(interactions$node_from, pattern = ' |[[:punct:]]', replacement = '_')
interactions$node_to <- gsub(interactions$node_to, pattern = ' |[[:punct:]]', replacement = '_')
interactions$node_from <- gsub(interactions$node_from, pattern = ' $|[[:punct:]]$', replacement = '')
interactions$node_to <- gsub(interactions$node_to, pattern = ' $|[[:punct:]]$', replacement = '')

interactions <- interactions %>% pivot_longer(!interaction_id, names_to = 'attribute', values_to = 'value')



write_csv(interactions, 'interactions.csv')

#nodes

nodes_pre <- NULL
for(i in 37:38){
  name<-paste0("M_PL_0",i,"-species.csv")
  nodes_pre[[i-36]]<-read.csv((name))
}
nodes_pre[[3]]<-read.csv(('M_PL_043-species.csv'))


plants<-unique(interactions[interactions$attribute == 'node_from',]$value)
pollinatiors<-unique(interactions[interactions$attribute == 'node_to',]$value)
nodes<-tibble(node_id=1:(length(plants)+length(pollinatiors)),node_name=c(plants,pollinatiors),
              type=c(rep("plant",length(plants)),rep("pollinator",length(pollinatiors))))

exp_nodes <- data.frame(node_id = 1:(length(plants)+length(pollinatiors)),node_name = c(plants,pollinatiors), type = c(rep.int('plant',length(plants)),rep.int('pollinator',length(pollinatiors))))



#physical nodes
nodes <- exp_nodes %>%
  pivot_longer(!node_id,names_to = 'attribute',values_to = 'value')


write_csv(nodes, 'nodes.csv')

#taxa verification
taxa_verification_parse(list(getwd()))


#state nodes
nodes_pre1 <- rbind(cbind(data.frame(layer_id = 1),nodes_pre[[1]]),cbind(data.frame(layer_id = 2),nodes_pre[[2]]),cbind(data.frame(layer_id = 3),nodes_pre[[3]]))
nodes_pre1 <- nodes_pre1 %>%
  group_by(Specie) %>%
  arrange(Specie)


colnames(nodes_pre1) <- gsub(colnames(nodes_pre1), pattern = '[[:punct:]]',replacement =  '_')
colnames(nodes_pre1) <- gsub(colnames(nodes_pre1),pattern =  ' $|[[:punct:]]$',replacement =  '')

nodes_pre1$Specie <- gsub(nodes_pre1$Specie, pattern = '[[:punct:]]| ',replacement =  '_')
nodes_pre1$Specie <- gsub(nodes_pre1$Specie, pattern =  ' $|[[:punct:]]$',replacement =  '')



filter(nodes, attribute == 'node_name', value == nodes_pre1$Specie[[2]])$node_id

state_nodes <- NULL
for (i in 1:nrow(nodes_pre1)) { 
  if (nodes_pre1$Specie[[i]] %in% nodes[nodes$attribute == 'node_name',]$value) {
  state_nodes <- rbind(state_nodes, data.frame(layer_id = nodes_pre1$layer_id[[i]],node_id = filter(nodes, attribute == 'node_name', value == nodes_pre1$Specie[[i]])$node_id,
                                               attribute = 'node_name', value = nodes_pre1$Specie[[i]]))
  state_nodes <- rbind(state_nodes, data.frame(layer_id = nodes_pre1$layer_id[[i]],node_id = filter(nodes, attribute == 'node_name', value == nodes_pre1$Specie[[i]])$node_id,
                                               attribute = 'degree', value = nodes_pre1$Degree[[i]]))
  state_nodes <- rbind(state_nodes, data.frame(layer_id = nodes_pre1$layer_id[[i]],node_id = filter(nodes, attribute == 'node_name', value == nodes_pre1$Specie[[i]])$node_id,
                                               attribute = 'connections_strength', value = nodes_pre1$Sum_connections_strength[[i]]))
  state_nodes <- rbind(state_nodes, data.frame(layer_id = nodes_pre1$layer_id[[i]],node_id = filter(nodes, attribute == 'node_name', value == nodes_pre1$Specie[[i]])$node_id,
                                               attribute = 'presence', value = nodes_pre1$Networks_presence[[i]]))
  } else {
    next
    }
  }

state_nodes <- state_nodes %>%
  group_by(layer_id, node_id) %>%
  arrange(node_id)

write_csv(state_nodes, 'state_nodes.csv')
#references
references<-tibble(doi=NA,author="Montero AC",year="01/01/2005",paper_url="http://mit.biology.au.dk/~biojmo/",data_url='http://www.web-of-life.es/map.php')
write_csv(references, '~/GitHub/emln_db/EMLN_Datasets/ofir_segev/spatial_Montero2005/references.csv')




#add the fact that there is a state nodes file to the description file manually
description <- read_csv('description.csv')
description[description$attribute == 'state_nodes',]$value <- as.logical('TRUE')

write_csv(description,'description.csv')
