library(tidyverse)
library(bipartite)

data<-read.csv("ScientificNamesANDCodesPlantAphidParasitoids.csv")
data$Code.plants<- as.numeric(data$Code.plants)
data$Aphid.Family<-str_trim(data$Aphid.Family,"both")
data$parasitoid<-str_trim(data$parasitoid,"both")
data$aphid<-str_trim(data$aphid,"both")
data$plants<-str_trim(data$plants,"both")

#layers
sites<-unique(data$Site)
layers<-NULL
for(i in 1:length(sites)){
  layers<-rbind(layers,tibble(layer=i,attribute='name',value=sites[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Melian2015/layers.csv')


#interactions
interactions<-tibble(node_from=data$parasitoid,layer_from=data$Site,
                     node_to=data$aphid, layer_to=data$Site,
                     weight=1,type="host-parasite",attribute1="category",value1="parasite-aphid")
interactions<-rbind(interactions,tibble(node_from=data$aphid,layer_from=data$Site,
                                        node_to=data$plants, layer_to=data$Site,
                                        weight=1,type="trophic",attribute1="category",value1="aphid-plant"))
interactions<-interactions %>% distinct()
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Melian2015/interactions.csv')


#nodes
parasitoids<-unique(data[c("parasitoid","Code.parasitoid","Aphid.Family")])
aphids<-unique(data[c("aphid","Code.aphid")])
plants<-unique(data[c("plants","Code.plants")])
nodes<-tibble(node_name=parasitoids$parasitoid,type="parasitoid",attribute1="code",value1=parasitoids$Code.parasitoid,attribute2="family",value2=parasitoids$Aphid.Family)
nodes<-rbind(nodes,tibble(node_name=aphids$aphid,type="aphid",attribute1="code",value1=aphids$Code.aphid,attribute2=NA,value2=NA))
nodes<-rbind(nodes,tibble(node_name=plants$plants,type="plant",attribute1="code",value1=plants$Code.plants,attribute2=NA,value2=NA))
nodes<- cbind(node_id=1:nrow(nodes),nodes)
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Melian2015/nodes.csv')


#references
references<-tibble(doi="10.1086/679505",author="Carlos J. Melian",year="14/01/2015")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Melian2015/references.csv')




