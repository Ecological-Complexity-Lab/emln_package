source("~/GitHub/emln_db/data_structure_functions.R")

#layers
sites<-read.csv("Sites.csv")
sites$sites <- trimws(sites$sites)
layers<-NULL
for(i in 1:nrow(sites)){
  layers<-rbind(layers,tibble(layer=i,attribute='name',value=str_replace_all(sites$sites[i],pattern = ' ',replacement = '_')))
  layers<-rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers<-rbind(layers,tibble(layer=i,attribute='latitude',value=sites$latitude[i]))
  layers<-rbind(layers,tibble(layer=i,attribute='longitude',value=sites$longitude[i]))
}

write_csv(layers, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Trojelsgaard2015/ofir/layers.csv')


#interactions
generate_tables<-function(link,layer){
  site<-read.csv(paste0(link,'.csv'))
  rnames=site$X
  site=site[,-1]
  site=data.matrix(site)
  rownames(site)=rnames
  
  site_list<-matrix_to_list_bipartite(site, group_names = c('pollinator','plant'))
  interactions<-site_list$edge_list
  interactions_site<-tibble(node_from=interactions$from,layer_from=layer,node_to=interactions$to,layer_to=layer,weight=interactions$weight,type="pollination")
  return(interactions_site)
}

s1 <- generate_tables('Site1_WesternSahara1', 'Western Sahara site 1')
s2 <- generate_tables('Site2_WesternSahara2', 'Western Sahara site 2')
s3 <- generate_tables('Site3_Fuerteventura1', 'Fuerteventura site 1')
s4 <- generate_tables('Site4_Fuerteventura2', 'Fuerteventura site 2')
s5 <- generate_tables('Site5_GranCanaria1', 'Gran Canaria site 1')
s6 <- generate_tables('Site6_GranCanaria2', 'Gran Canaria site 2')
s7 <- generate_tables('Site7_TenerifeSouth1', 'Tenerife South site 1')
s8 <- generate_tables('Site8_TenerifeSouth2', 'Tenerife South site 2')
s9 <- generate_tables('Site9_TenerifeTeno1', 'Tenerife Teno site 1')
s10 <- generate_tables('Site10_TenerifeTeno2', 'Tenerife Teno site 2')
s11 <- generate_tables('Site11_Gomera1', 'La Gomera site 1')
s12 <- generate_tables('Site12_Gomera2', 'La Gomera site 2')
s13 <- generate_tables('Site13_Hierro1', 'El Hierro site 1')
s14 <- generate_tables('Site14_Hierro2', 'El Hierro site 2')

all_interactions<-rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14)

for (i in colnames(all_interactions)) {
  all_interactions[[i]] <- gsub(all_interactions[[i]], pattern = ' $', replacement = '')
  all_interactions[[i]] <- gsub(all_interactions[[i]], pattern = ' +|[[:punct:]]+|[[:punct:]] +', replacement = '_')
  all_interactions[[i]] <- gsub(all_interactions[[i]], pattern = ' +', replacement = '_')
  all_interactions[[i]] <- gsub(all_interactions[[i]], pattern = '[[:punct:]]+$', replacement = '')
}

all_interactions <- cbind(data.frame(interaction_id = 1:969),all_interactions)

interactions <- all_interactions %>%
  pivot_longer(!interaction_id, names_to = 'attribute',values_to = 'value')


write_csv(all_interactions, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Trojelsgaard2015/ofir/interactions.csv')


#nodes
pollinators_unique <- unique(all_interactions$node_to)
plants_unique <- unique(all_interactions$node_from)
all_nodes <- tibble(node_id=1:(length(pollinators_unique)+length(plants_unique)),
                         node_name=c(pollinators_unique,plants_unique),
                         type=c(rep('pollinator',length(pollinators_unique)),rep('plant',length(plants_unique))))
write_csv(all_nodes, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Trojelsgaard2015/ofir/nodes.csv')


#reference
reference <- tibble(doi='10.1098/rspb.2014.2925',author='Trøjelsgaard K.', year="2015")
write_csv(reference, '/Users/ofir/GitHub/emln_db/EMLN_Datasets/Spatial_Trojelsgaard2015/ofir/references.csv')

