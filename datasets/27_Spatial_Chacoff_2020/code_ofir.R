library(tidyverse)
library(bipartite)
library(readxl)

insect_names<-read_excel("plant_pollinator_data.xlsx", sheet="insect_names")
plant_names<-read_excel("plant_pollinator_data.xlsx", sheet="plant_names")
flower_abundance<-read_excel("plant_pollinator_data.xlsx", sheet="Flower_abundance")
phenological_overlap<-read_excel("plant_pollinator_data.xlsx", sheet="phenological_overlap")
plant_traits<-read_excel("plant_pollinator_data.xlsx", sheet="plant_traits")
pollinator_traits<-read_excel("plant_pollinator_data.xlsx", sheet="pollinator_traits")
interactions_site1_2006<-read_excel("plant_pollinator_data.xlsx", sheet="interactions_site1_2006")
interactions_site2_2006<-read_excel("plant_pollinator_data.xlsx", sheet="interactions_site2_2006")
interactions_site3_2006<-read_excel("plant_pollinator_data.xlsx", sheet="interactions_site3_2006")
interactions_site4_2006<-read_excel("plant_pollinator_data.xlsx", sheet="interactions_site4_2006")

#layers
layers<-NULL
latitudes<-c(-32.629722,-32.749722,-32.639444,-32.629167)
longitudes<-c(-69.008333,-69.175278,-69.031111,-69.018333)
for(i in 1:4){
  
    layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
    layers<-rbind(layers,tibble(layer=i,attribute='name',value=paste0('site',i,'_2006')))
    layers<-rbind(layers,tibble(layer=i,attribute='location',value="Villavicencio Nature Reserve, Mendoza, Argentina"))
    layers<-rbind(layers,tibble(layer=i,attribute='year',value="2006"))
    layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=latitudes[i]))
    layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=-longitudes[i]))
}
write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Chacoff2020/layers.csv')

#interactions
generate_table<-function(data,layer){
  output=NULL
  for(i in 1:nrow(data)){
    for(j in 2:ncol(data)){
      square<-data[[i,j]]
      if(square!=0){
          output<-rbind(output,tibble(node_from=colnames(data)[[j]],layer_from=layer,node_to=data$...1[[i]],layer_to=layer,weight=square,type="pollination"))
      }
    }
  }
  return(output)
}
interactions=NULL
datasets<-list(interactions_site1_2006,interactions_site2_2006,interactions_site3_2006,interactions_site4_2006)
               
for(i in 1:4){
    interactions<-rbind(interactions,generate_table(datasets[[i]],i))
}
convert_shortcuts<-function(code){
  if(code %in% insect_names$pol_code)
    name=filter(insect_names,pol_code==code)$species_name
  else
    name=filter(plant_names,plant_code==code)$species_name
  return(name)
}
for(i in 1:nrow(interactions)){
  interactions$node_from[i]<-convert_shortcuts(interactions$node_from[i])
  interactions$node_to[i]<-convert_shortcuts(interactions$node_to[i])
}
write_csv(interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Chacoff2020/interactions.csv')

#nodes
nodes<-tibble(node_name=plant_names$species_name,type="plant",attribute1="code", value1=plant_names$plant_code,
              attribute2="corolla_length",value2=plant_traits$corolla_length,attribute3="corolla_aperture",value3=plant_traits$corolla_aperture,
              attribute4="height_mean",value4=plant_traits$height_mean,attribute5="flower_abundance",value5=flower_abundance$flower_abtot,
              attribute6=NA,value6=NA)
nodes<-rbind(nodes,tibble(node_name=insect_names$species_name,type="pollinator",attribute1="code", value1=insect_names$pol_code,
                          attribute2="body_length",value2=pollinator_traits$body_length,attribute3="proboscis_length",value3=pollinator_traits$proboscis_length,
                          attribute4="proboscis_width",value4=pollinator_traits$proboscis_width,attribute5="body_width",value5=pollinator_traits$body_width,
                          attribute6="body_thickness",value6=pollinator_traits$body_thickness))
nodes<-cbind(tibble(node_id=1:nrow(nodes)),nodes)
write_csv(nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Chacoff2020/nodes.csv')

#references
references<-tibble(doi="10.5061/dryad.8cz8w9gm1",author="Chacoff, Natacha P",year="14/10/2020")
write_csv(references, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir/Spatial_Chacoff2020/references.csv')



