library(tidyverse)
library(bipartite)
library(readxl)

clean_xls<-function(data){
  #parasite column table
  parasites<-data[1:2,4:ncol(data)]
  colnames(parasites)<-as.character(data[3,4:ncol(data)])
  #make table with names headers
  data<-data[3:nrow(data),]
  parasites_names<-paste0(parasites[1,],"_",parasites[2,])
  col<-c(data[1,1:3],parasites_names)
  colnames(data)<-as.character(col)
  data<-data[-c(1),]
  output<-list(data,parasites)
  return(output)
}

datasets_p<-list()
parasites_p<-list()
datasets_i<-list()
parasites_i<-list()
names<-c("aishihik_lake","cold_lake","lake_of_the_woods","mcgregor_river","parsnip_river","sbay_lake_huron","smallwood_reservoir")
for(i in 1:7){
  name<-paste0(names[i],".xls")
  print(name)
  output<-clean_xls(read_excel(name, col_names = FALSE,sheet = "prevalence")[,-3])
  datasets_p[[names[i]]] <- output[[1]]
  parasites_p[[i]]<-output[[2]]
  if(i!=3 & i!=6){
    output<-clean_xls(read_excel(name, col_names = FALSE,sheet = "intensity")[,-3])
    datasets_i[[i]]<-output[[1]]
    parasites_i[[i]]<-output[[2]]

  }
}

View(read_excel('aishihik_lake.xls', col_names = FALSE,sheet = "prevalence")[,-3])

#layers
layers=NULL
latitudes<-c(61.228250,54.464180,48.161440,45.455000,50.023180,44.826883,54.170220)
longtitudes<-c(-136.962610,-110.182259,-83.655790,-63.246310,-125.249540,-82.428222,-64.3797157)
for(i in 1:7){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space & interaction'))
  layers<-rbind(layers,tibble(layer=i,attribute='interaction_type',value="prevalence"))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=names[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=latitudes[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=longtitudes[i]))
}
layer_num=8
for(i in 1:7) {
  if(i!=3 & i!=6){
    layers<-rbind(layers,tibble(layer=layer_num,attribute='type',value='space & interaction'))
    layers<-rbind(layers,tibble(layer=layer_num,attribute='interaction_type',value="intensity"))
    layers<-rbind(layers,tibble(layer=layer_num,attribute='location',value=names[i]))
    layers<-rbind(layers,tibble(layer=layer_num,attribute='latitude',value=latitudes[i]))
    layers<-rbind(layers,tibble(layer=layer_num,attribute='longitude',value=longtitudes[i]))
    layer_num<-layer_num+1
  }
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Canadian_fish_parasite_iwdb/layers.csv')

#interactions
generate_table<-function(data,layer,category){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 4:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        output<-rbind(output,tibble(node_from=data$`Host species`[[i]],layer_from=layer,node_to=colnames(data)[[j]],layer_to=layer,weight=square,type="host-parasite",
                                    attribute1="category",value1=category))
      }
    }
  }
  return(output)
}
interactions=NULL
for(i in 1:7){
  interactions<-rbind(interactions,generate_table(datasets_p[[i]],i,"prevalence"))
}
layer_num=8
for(i in 1:7){
  if(i!=3 & i!=6){
    interactions<-rbind(interactions,generate_table(datasets_p[[i]],layer_num,"intensity"))
    layer_num<-layer_num+1
  }
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Canadian_fish_parasite_iwdb/interactions.csv')

interactions <- cbind(tibble('interaction_id' = 1:nrow(interactions)),interactions)
interactions$layer_from <- as.character(interactions$layer_from)
interactions$layer_to <- as.character(interactions$layer_to)
colnames(interactions)[9] <- 'category'
interactions <- interactions[,-8]

interactions <-  interactions %>%
  pivot_longer(!interaction_id, names_to = 'attribute', values_to = 'value', names_transform = list(layer_from = as.character))

write_csv(interactions, '~/GitHub/emln_db/EMLN_Datasets/datasets/24_Spatial_Canadian-fish-parasite/interactions.csv')

#nodes
find_num_of_hosts<-function(name) {
  output=""
  for(i in 1:7){
    num<-filter(datasets_p[[i]],`Host species`==name)$`Sample size`
    if(is_empty(num)){
      num<-0
    }
    output<-paste0(output,num,",")
  }
  for(i in 1:7){
    if(i!=3 & i!=6){
      num<-filter(datasets_i[[i]],`Host species`==name)$`Sample size`
      if(is_empty(num)){
        num<-0
      }
      if(i!=7){
        output<-paste0(output,num,",")
      }
      else{
        output<-paste0(output,num)
      }
    }
  }
  return(output)
}



nodes<-NULL
hosts<-unique(interactions$node_from)
#for(i in 1:length(hosts)){
#  nodes<-rbind(nodes,tibble(node_name=hosts[i],type="host",attribute1="sample_size",value1=find_num_of_hosts(hosts[i]),
 #                           attribute2=NA,value2=NA))
#}

for (i in 1:length(hosts)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=str_replace_all(hosts[i],' ','_')))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= 'host'))
}

para<-unique(interactions$node_to)
#for(i in 1:length(para)){
 # nodes<-rbind(nodes,tibble(node_name=unique(interactions$node_to)[i],type="parasite",attribute1="genus",value1=para[i][[1]][1],
#                            attribute2="species",value2=para[i][[1]][2]))
#}

for (i in 1:length(para)) {
  nodes <- rbind(nodes,tibble(node_id=i+length(hosts),attribute='node_name',value=str_replace_all(para[i],' ','_')))
  nodes <- rbind(nodes,tibble(node_id=i+length(hosts),attribute='type',value= 'parasite'))

}
#nodes<-cbind(tibble(node_id=1:nrow(nodes)),nodes)
#write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Canadian_fish_parasite_iwdb/nodes.csv')

write_csv(nodes, paste(getwd(),'nodes.csv',sep = '/'))


#state nodes
find_num <- function(name, layer_location) {
  numbers <- list()
  for (i in 1:7) {
    num<-as.numeric(filter(datasets_p[[layer_location]],`Host species`==name)$`Sample size`)
    numbers <- c(numbers,num)
    if(is_empty(num)){
      num<-0
    }
  }
  return(num)
}

state_nodes <- NULL

for (i in 1:max(nodes$node_id)) {
  for (j in 1:7) {
    layer_location <- unique(filter(layers, attribute == 'location' & value == names[[j]])$value)
    node_name <- str_replace_all(filter(nodes, node_id == i & attribute == 'node_name')$value,'_',' ')
    layer_id <- filter(layers, attribute == 'location' & value == layer_location)$layer
    for (x in layer_id) {
      state_nodes <- rbind(state_nodes, tibble(layer_id = x, node_id = i, attribute = c('node_name','sample_size'), value = c(node_name,find_num(node_name, layer_location))))
    }
  }
}

write_csv(state_nodes, 'state_nodes.csv')
#references
references<-tibble(doi="10.1139/f83-194",author="Arai, H. P.",year="04/10/1983")
references<-rbind(references,tibble(doi="10.1139/f76-294",author="J. R. Arthur",year="01/11/1976"))
references<-rbind(references,tibble(doi="10.2307/2422308",author="Bangham, R. V.",year="01/01/1955"))
references<-rbind(references,tibble(doi="10.1111/j.1095-8649.1978.tb03427.x",author="Chinniah V.C, William Threlfall",year="01/08/1978"))
references<-rbind(references,tibble(doi="10.1139/f72-046",author="Alex O. Dechtiar",year="01/03/1972"))
references<-rbind(references,tibble(doi="10.1111/j.1095-8649.1981.tb03811.x",author="T. S. Leong, J. C. Holmest",year="01/06/1981"))
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Canadian_fish_parasite/references.csv')







#noa fix
nodes <- read_csv('nodes.csv') %>% pivot_wider(id_cols = node_id, names_from = attribute, values_from = value)
interactions <- read_csv('interactions.csv') %>% pivot_wider(id_cols = interaction_id, names_from = attribute, values_from = value)

unique(c(interactions$node_from,interactions$node_to)) %in% nodes$node_name

nodes$node_name[!nodes$node_name%in%unique(c(interactions$node_from,interactions$node_to))]
