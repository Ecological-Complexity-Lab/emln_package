library(tidyverse)
library(bipartite)
library("readxl")

Populations<-read_excel("Populations.xlsx")   #layers (space + time)
ListFleas<-read_excel("ListFleas.xlsx")   # (names and shorts)
ListMammals<-read_excel("ListMammals.xlsx")   # (names and shorts)
ListMites<-read_excel("ListMites.xlsx")   # (names and shorts)
ListHosts<-read_excel("ListHosts.xlsx")   # list of individuals
Fleas<-read_excel("Fleas.xlsx")   # host-flea interactions
Mites<-read_excel("Mites.xlsx")   # host-mite interactions

#layers
layers<-NULL
for(i in 1:nrow(Populations)){
  layers<-rbind(layers,tibble(layer=i,attribute='type',value=c('space','time')))
  layers<-rbind(layers,tibble(layer=i,attribute='population_no',value=Populations$Population[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=Populations$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=Populations$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='biotop',value=Populations$Biotop[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='region',value=Populations$Region[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='locality',value=Populations$Locality[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='files',value=Populations$Files[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='year',value=Populations$Year[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='month',value=Populations$Month[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='first_date',value=as.character(Populations$FirstDate[i])))
  layers<-rbind(layers,tibble(layer=i,attribute='last_date',value=as.character(Populations$LastDate[i])))
  layers<-rbind(layers,tibble(layer=i,attribute='num_traps',value=Populations$NumTraps[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='nights',value=Populations$Nights[i]))
}
write_csv(layers, '~/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Slovakia/layers.csv')

write_csv(layers,'layers.csv')
#interactions - ignore biot1,biot2,

generate_table<-function(data,layer_type){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 10:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
        layer = filter(layers, attribute == 'population_no', value == data$Population[[i]])$layer
        sex=data$Sex[[i]]
        body_mass=data$`Body mass`[[i]]
        number=data[[i,1]]
        from_name=filter(ListMammals,HostShortName==data$Host[i])[["HostSciName"]]
        if(layer_type=="fleas"){
          to_name=filter(ListFleas,FleaShortName==colnames(data)[j])[["FleaSciName"]]
        }
        else if(layer_type=="mites"){
          to_name=filter(ListMites,MiteShortName==colnames(data)[j])[["MiteSciName"]]
        }
        output<-rbind(output,tibble(node_from=from_name,layer_from=layer,node_to=to_name,layer_to=layer,weight=square,type="host-parasite",
                                    ))
      }
    }
  }
  return(output)
}

interactions<-generate_table(Fleas,"fleas")
Mites$Vek<-NULL
Mites<- Mites %>% rename(Sex=Pohl ,`Body mass`=Hmot)
interactions<-rbind(interactions, generate_table(Mites,"mites"))
write_csv(interactions, '~/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Slovakia/interactions.csv')

interactions <- rename(interactions, 'category' = value1, 'host_serial_num' = value2, 'sex' = value3, 'body_mass' = value4)
interactions <- interactions %>% select(-starts_with('attribute'))
interactions <- cbind(interaction_id = 1:nrow(interactions), interactions)

interactions$layer_from <- as.character(interactions$layer_from)
interactions$layer_to <- as.character(interactions$layer_to)
interactions$weight <- as.character(interactions$weight)

inter <- pivot_longer(interactions, !interaction_id, names_to = 'attribute', values_to = 'value')
inter$value <- gsub(inter$value, pattern = ' ',replacement = '_')
write.csv(file = 'interactions.csv',x = inter, row.names = FALSE)


#nodes
nodes<-tibble(node_name=ListMammals$HostSciName,type="host",attribute1="short_name", value1=ListMammals$HostShortName,
              attribute2="determinator", value2=ListMammals$HostDeterminator, attribute3="groupIR", value3=ListMammals$GroupIR,
              attribute4="host_type", value4=ListMammals$HostType, attribute5="exclude", value5=ListMammals$Exclude)
nodes<-rbind(nodes,tibble(node_name=ListFleas$FleaSciName,type="parasite",attribute1="short_name", value1=ListFleas$FleaShortName,
                          attribute2="determinator", value2=ListFleas$FleaDeterminator,
                          attribute3=NA,value3=NA,attribute4=NA,value4=NA,attribute5=NA,value5=NA))
nodes<-rbind(nodes,tibble(node_name=ListMites$MiteSciName,type="parasite",attribute1="short_name", value1=ListMites$MiteShortName,
                          attribute2="determinator", value2=ListMites$MiteDeterminator,
                          attribute3=NA,value3=NA,attribute4=NA,value4=NA,attribute5=NA,value5=NA))
nodes<-rbind(tibble(node_id=1:nrow(nodes),nodes))
write_csv(nodes, '~/GitHub/emln_db/EMLN_Datasets/Spatial_Slovakia/nodes.csv')


interactions


#references

#attribute1="category",value1=layer_type, attribute2="host_serial_num", value2=number,attribute3="sex", value3=sex, attribute4="body_mass", value4=body_mass)



###ןinteractions fix###

interactions_nodes <- unique(c(interactions$node_from,interactions$node_to))
nodes <- read.csv('nodes.csv')
unique(nodes[nodes$attribute == 'node_name',]$value)

nodes[nodes$attribute == 'node_name',]$value[!nodes[nodes$attribute == 'node_name',]$value %in% interactions_nodes]


interactions_nodes %in% nodes[nodes$attribute == 'node_name',]$value
                                                                    

interactions_nodes[78] %in% nodes[nodes$attribute == 'node_name',]$value


all(unique(c(inter[inter$attribute == 'node_from',]$value,inter[inter$attribute == 'node_to',]$value)) %in% nodes[nodes$attribute == 'node_name',]$value)
