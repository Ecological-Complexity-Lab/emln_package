library(tidyverse)
library(bipartite)

# function for cleaning species names from the layer name in the end 
# x is a vector of all layers names
clean_data<-function(data,x){
  for(i in 1:nrow(data)){
    for(j in 1:length(x)){
      data$node_from[[i]]<-str_remove(data$node_from[[i]],x[j])
      data$node_to[[i]]<-str_remove(data$node_to[[i]],x[j])
    }
  }
  return(data)
}

amami_data<-read.csv("Amami-Ohsima_Island.csv")
ashu_data<-read.csv("Ashu.csv")
kibune_data<-read.csv("Kibune.csv")
kyoto_data<-read.csv("Kyoto_City.csv")
kushigata_data<-read.csv("Mt.Kushigata.csv")
yufu_data<-read.csv("Mt.Yufu.csv")
nakaikemi_data<-read.csv("Nakaikemi_marsh.csv")
reference_data<-rbind(read.csv("references_Amami-Ohsima_Island.csv"),
                      read.csv("references_Ashu.csv"),
                      read.csv("references_Kibune.csv"),
                      read.csv("references_Kyoto_City.csv"),
                      read.csv("references_Mt.Kushigata.csv"),
                      read.csv("references_Mt.Yufu.csv"),
                      read.csv("references_Nakaikemi_marsh.csv"))

#layers
layers=NULL
names<-c("Amami-Ohsima_Island","Ashu","Kibune","Kyoto_City","Mt.Kushigata","Mt.Yufu","Nakaikemi_marsh")
years<-c("1996-1999","1984-1987","1984-1987","1985-1987","1990-1991","2001","1994-1995")
for(i in 1:7){
  layers<-rbind(layers,tibble(layer=i,attribute='name',value=names[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='location',value=reference_data$Locality.of.Studi[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=reference_data$Latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=reference_data$Longitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='years',value=years[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_species',value=reference_data$Species[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='num_of_interactions',value=reference_data$Interactions[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='connectance',value=reference_data$Connectance[i]))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Kyoto/layers.csv')

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
datasets<-list(amami_data,ashu_data,kibune_data,kyoto_data,kushigata_data,yufu_data,nakaikemi_data)
for(i in 1:7){
  interactions<-rbind(interactions,generate_table(datasets[[i]],names[i]))
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Kyoto/interactions.csv')

#nodes
plants<-unique(interactions$node_from)
polinators<-unique(interactions$node_to)
nodes<-tibble(node_id=1:(length(plants)+length(polinators)),node_name=c(plants,polinators),
              type=c(rep("plant",length(plants)),rep("pollinator",length(polinators))))
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Kyoto/nodes.csv')

#references
references=NULL
references<-rbind(references,tibble(doi=NA,author="KATO, Makoto",year="31/03/2000",paper_url="http://hdl.handle.net/2433/156116",data_url='http://www.web-of-life.es/map.php')) #Amami-Ohsima Island
references<-rbind(references,tibble(doi=NA,author="KATO, Makoto; KAKUTANI, Takehiko; INOUE, Tamiji; ITINO, Takao",year="20/08/1990",paper_url="http://hdl.handle.net/2433/156101",data_url=c('http://www.web-of-life.es/map.php','http://www.ecologia.ib.usp.br/iwdb/resources.html'))) #Ashu
references<-rbind(references,tibble(doi=NA,author="INOUE, Tamiji; KATO, Makoto; KAKUTANI, Takehiko; SUKA, Takeshi; ITINO, Takao",year="20/08/1990",paper_url="http://hdl.handle.net/2433/156100",data_url = 'http://www.web-of-life.es/map.php')) #Kibune
references<-rbind(references,tibble(doi=NA,author="KAKUTANI, Takehiko; INOUE, Tamiji; KATO, Makoto; ICHIHASHI, Hideyuki",year="20/08/1990",paper_url="http://hdl.handle.net/2433/156099",data_url='http://www.web-of-life.es/map.php')) #Kyoto City
references<-rbind(references,tibble(doi=NA,author="KATO, Makoto; MATSUMOTO, Masamichi; KATO, Tôru",year="31/03/1993",paper_url="http://hdl.handle.net/2433/156107",data_url='http://www.web-of-life.es/map.php')) #Mt. Kushigata
references<-rbind(references,tibble(doi=NA,author="YAMAZAKI, Kyoko; KATO, Makoto",year="21/04/2003",paper_url="http://hdl.handle.net/2433/156407",data_url='http://www.web-of-life.es/map.php')) #Mt. Yufu
references<-rbind(references,tibble(doi=NA,author="KATO, Makoto; MIURA, Reiichi",year="31/03/1996",paper_url="http://hdl.handle.net/2433/156114",data_url='http://www.web-of-life.es/map.php')) #Nakaikemi marsh
write_csv(references, '~/GitHub/emln_db/EMLN_Datasets/ofir_segev/Spatial_Kyoto/references.csv')

