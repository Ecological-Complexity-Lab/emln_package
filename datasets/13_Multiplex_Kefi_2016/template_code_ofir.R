library(tidyverse)
library(bipartite)
library("readxl")

metadata<-read_excel("chilean_metadata.xls")
data_NTIneg<-read_delim("chilean_NTIneg.txt", delim = '\t')
data_NTIpos<-read_delim("chilean_NTIpos.txt", delim = '\t')
data_TI<-read_delim("chilean_TI.txt", delim = '\t')

#layers
layers<-NULL
names<-c("non trophic negative interactions","non trophic positive interactions","trophic interactions")
for(i in 1:3){
  layers<-rbind(layers,tibble(layer=i,attribute='name',value=names[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='interaction'))
  layers<-rbind(layers,tibble(layer=i,attribute='directed',value=TRUE))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Multiplex_Kefi2016/layers.csv')

#interactions
generate_table<-function(data,layer,layer_type){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 3:ncol(data)){
      square<-data[[i,j]]
      if(square==1){
        from_name=filter(metadata,Spec==data$X1[i])[[2]]
        to_name=filter(metadata,Spec==colnames(data)[j])[[2]]
        output<-rbind(output,tibble(node_from=from_name,layer_from=layer,node_to=to_name,layer_to=layer,weight=1,type=layer_type))
      }
   }
  }
  return(output)
}

t1<-generate_table(data_NTIneg,"NTIneg","negative")
t2<-generate_table(data_NTIpos,"NTIpos","positive")
t3<-generate_table(data_TI,"TI","trophic")
interactions=rbind(t1,t2,t3)
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Multiplex_Kefi2016/interactions.csv')

#nodes
nodes<-tibble(node_id=1:nrow(metadata),node_name=metadata$`Species names`,type=metadata$trophic,attribute1="code",value1=metadata$Spec,
              attribute2="body_mass",value2=metadata$BodyMass,attribute3="sessile/mobile",value3=metadata$`sessile/mobile`,
              attribute4="cluster",value4=metadata$Cluster,attribute5="shore_height",value5=metadata$`Shore Height 1 conservative`,
              attribute6="shoreHt_C_Ordinal",value6=metadata$ShoreHt_C_Ordinal,attribute7="shoreHt_C_Breadth",value7=metadata$ShoreHt_C_Breadth,
              attribute8="phyllum",value8=metadata$Phyllum,attribute9="subphyllum",value9=metadata$subphyllum)
v<-is.na(nodes$type)
nodes$type[v]<-"Plankton"
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Multiplex_Kefi2016/nodes.csv')

#references
references<-tibble(doi="10.1371/journal.pbio.1002527",author="Sonia Kéfi",year="03/08/2016")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Multiplex_Kefi2016/references.csv')



