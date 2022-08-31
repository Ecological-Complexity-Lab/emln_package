library(tidyverse)
library(bipartite)

data<-read.csv("caradonna_rmbl_interaction_networks_data_EDI.csv")
data_layers<-tibble(year=data$year,week=data$week_num,date=data$date,site=data$site)
data_layers<-unique(data_layers)
 #layers

layers=NULL
layer_num=1
sites=c("shady_point","twin_meadows")
years=c(2013,2014,2015)
layers_wide=NULL
for(i in 1:16){
  for(j in 1:length(years)){
    for(k in 1:length(sites)){
      x=filter(data_layers,week==i,year==years[j],site==sites[k])
      if(nrow(x)>0){
        x<-x[order(x$date),]
        layers<-rbind(layers,tibble(layer=layer_num,attribute='type',value='space & time'))
        layers<-rbind(layers,tibble(layer=layer_num,attribute='location',value="Rocky Mountain Biological Laboratory,Gothic,CO,USA"))
        layers<-rbind(layers,tibble(layer=layer_num,attribute='site',value=sites[k]))
        layers<-rbind(layers,tibble(layer=layer_num,attribute='week',value=i))
        layers<-rbind(layers,tibble(layer=layer_num,attribute='year',value=years[j]))
        start_date=x$date[[1]]
        layers<-rbind(layers,tibble(layer=layer_num,attribute='start_date',value=paste0(substr(start_date,9,10),'/',substr(start_date,6,7),'/',substr(start_date,1,4))))
        end_date=x$date[[nrow(x)]]
        layers<-rbind(layers,tibble(layer=layer_num,attribute='end_date',value=paste0(substr(end_date,9,10),'/',substr(end_date,6,7),'/',substr(end_date,1,4))))
        if(k==1){
          layers<-rbind(layers,tibble(layer=layer_num,attribute='latitude',value=37.933364))
          layers<-rbind(layers,tibble(layer=layer_num,attribute='longitude',value=-107.9038794))
        }
        else{
          layers<-rbind(layers,tibble(layer=layer_num,attribute='latitude',value=39.150357))
          layers<-rbind(layers,tibble(layer=layer_num,attribute='longitude',value=-106.252863))
        }
        layers_wide<-rbind(layers_wide,tibble(id=layer_num,site=sites[k],week=i,year=years[j]))
        layer_num=layer_num+1
      }
    }
  }
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_CaraDona2020/layers.csv')

#interactions
id<-c()
for(i in 1:nrow(data)){
  id<-append(id,values=filter(layers_wide,site==data$site[[i]] ,week==data$week_num[[i]] ,year==data$year[[i]])$id)
}
interactions<-tibble(node_from=data$pollinator, layer_from=id, node_to=data$plant, layer_to=id, type="pollination",
                     weight=data$interactions, attribute1="date", value1=data$date,
                     attribute2="transect", value2=data$transect, attribute3="pollinator_sex", value3=data$pollinator_sex,
                     attribute4="start_time", value4=data$start_time, attribute5="end_time", value5=data$end_time)
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_CaraDona2020/interactions.csv')

#nodes
plants<-unique(data$plant)
pollinators<-tibble(name=data$pollinator, family=data$pollinator_family, group=data$pollinator_group)
pollinators<-unique(pollinators)
nodes<-tibble(node_name=pollinators$name, type="pollinator", attribute1="family", value1=pollinators$family, attribute2="group", value2=pollinators$group)
nodes<-rbind(nodes,tibble(node_name=plants, type="plant", attribute1=NA, value1=NA, attribute2=NA, value2=NA))
nodes<-cbind(tibble(node_id=1:nrow(nodes)),nodes)
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_CaraDona2020/nodes.csv')

#reference
references<-tibble(doi="10.6073/pasta/27dc02fe1655e3896f20326fed5cb95f",author="Paul CaraDonna",year="13/05/2020")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Temporal_CaraDona2020/references.csv')


