library(tidyverse)
library(bipartite)

datasets<-list()
for(i in 1:51){
  if(i<10){
    name<-paste0("A_HP_00",i,".csv")
  }
  else{
    name<-paste0("A_HP_0",i,".csv")
  }
  datasets[[i]]<-read.csv((name))
}
reference_data<-read.csv("references_data.csv")

#layers
layers=NULL
for(i in 1:51){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=reference_data$Locality.of.Studi[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=reference_data$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=reference_data$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_species',value=reference_data$Species[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=reference_data$Interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='connectance',value=reference_data$Connectance[i]))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Hadfield2013/layers.csv')

#interactions
generate_table<-function(data,layer){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 3:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        output<-rbind(output,tibble(node_from=data$X[[i]],layer_from=layer,node_to=colnames(data)[[j]],layer_to=layer,weight=square,type="host-parasite"))
      }
    }
  }
  return(output)
}
interactions=NULL
for(i in 1:51){
  interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Hadfield2013/interactions.csv')

#nodes
hosts<-unique(interactions$node_from)
parasites<-unique(interactions$node_to)
nodes<-tibble(node_id=1:(length(hosts)+length(parasites)),node_name=c(hosts,parasites),
              type=c(rep("host",length(hosts)),rep("parasite",length(parasites))),attribute1=c(rep("hosts_sampled_per_network",length(hosts)),rep(NA,length(parasites))))

find_num_of_hosts<-function(name){
  output=""
  for(i in 1:51){
    num<-filter(datasets[[i]],X==name)$`Num..of.hosts.sampled`
    if(is_empty(num)){
      num<-0
    }
    if(i!=51){
      output<-paste0(output,num,",")
    }
    else{
      output<-paste0(output,num)
    }
  }
  return(output)
}
values<-c()
for(i in 1:length(hosts)){
  values[i]<-find_num_of_hosts(hosts[i])
}
nodes<-cbind(nodes,value1=c(values,rep(NA,length(parasites))))
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Hadfield2013/nodes.csv')

#references
references<-tibble(doi="10.1086/674445",author="Jarrod D. Hadfield",year="30/12/2013")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Hadfield2013/references.csv')






#nodes long
nodes <- nodes[,-4]
nodes$node_name <-   str_replace_all(nodes$node_name,pattern =  ' |[[:punct:]]',replacement = '_')

nodes %<>% pivot_longer(!node_id, names_to = 'attribute', values_to = 'value')

write_csv(nodes, 'nodes.csv')

#state nodes
find_num <- function(name, layer_location) {
  numbers <- list()
  for (i in 1:7) {
    num<-as.numeric(filter(datasets[[layer_location]],X==name)$`Num..of.hosts.sampled`)
    numbers <- c(numbers,num)
    if(is_empty(num)){
      num<-0
    }
  }
  return(num)
}

state_nodes <- NULL

for (i in 1:max(nodes$node_id)) {
  for (j in 1:51) {
    #layer_location <- unique(filter(layers, attribute == 'location' & value == names[[j]])$value)
    node_name <- str_replace_all(filter(nodes, node_id == i & attribute == 'node_name')$value,'_',' ')
    layer_id <- j
    for (x in layer_id) {
      state_nodes <- rbind(state_nodes, tibble(layer_id = x, node_id = i, attribute = c('node_name','sample_size'), value = c(node_name,find_num(node_name, j))))
    }
  }
}

write_csv(state_nodes, 'state_nodes.csv')

