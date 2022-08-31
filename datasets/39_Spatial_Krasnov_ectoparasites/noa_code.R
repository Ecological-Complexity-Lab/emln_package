library(tidyverse)
library(bipartite)
library(rstudioapi)
library(readxl)
source('~/GitHub/emln_db/data_structure_functions.R')
source('~/GitHub/emln_db/noa_functions.R')

#set working directory
set_file_d()

#layers
X_layers <- read_excel("layers.xlsx")
#remove lat lon
X_layers <- X_layers[,-4:-5]
##fix name error
#X_layers$name <- str_replace(X_layers$name, 'Zair','Zaire')

sources <- read_excel('network_source.xlsx')
sources <- na.omit(sources) ##remove the blank row that was in the original file

sources$Name <- str_replace(sources$Name, 'Zaire','Zair')

##### sort the X_layers and sources by the same variables so it will be compatible and I can add the source column to the X_layers dataframe
X_layers <- X_layers %>%
  group_by(network_code) %>%
  arrange(network_code)

sources <- sources %>%
  group_by(Network) %>%
  arrange(Network)

#now the sources has all the information
sources$Realm = X_layers$realm

layers <- NULL
for (i in 1:nrow(X_layers)){
  layers <- rbind(layers, tibble(layer=i,attribute='network_code', value=sources$Network[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='code_full_name',value=sources$Name[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='type',value='space'))
  layers <- rbind(layers, tibble(layer=i,attribute='realm',value=sources$Realm[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='latitude',value=sources$Latitude[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='longitude',value=sources$Longitude[i]))
  layers <- rbind(layers,tibble(layer=i,attribute='network_source',value=gsub(pattern = '[[:punct:]]',replacement = '',sources$Sources[i])))
  
}

write_csv(layers, 'layers.csv')

#interactions
interactions <- NULL
parent_directory <- '~/GitHub/emln_db/EMLN_Datasets/datasets/39_Spatial_Krasnov_ectoparasites'
list_of_directories <- list.files(path = parent_directory, full.names = F, recursive = TRUE)

#function that recieves the file name, reades it to check if its an interaction file, if it is it reads it, creates row names and uses infomapecology to turn the matrix to an edge list.
inter_table <- function (file_name) {
  for (i in 1:length(list_of_directories)) { 
    if (str_starts(list_of_directories[i], pattern = 'afro|nrct|ntrp|plct')) {
      layer_code <- gsub(x = file_name, pattern = '^.{5}|.{4}$', replacement = '') #get the layer codes from the csv name so I know in which layer the interacctions are
      countryx <- read_csv(file_name)
      countryx <- column_to_rownames(countryx,var =colnames(countryx)[1])
      interaction_yearx <- matrix_to_list_bipartite(countryx)$edge_list
      interactions <- rbind(interactions, tibble(node_from = interaction_yearx$from, layer_from = rep(filter(layers, attribute == 'network_code' & value == layer_code)$layer),
                                                 node_to = interaction_yearx$to, layer_to = rep(filter(layers, attribute == 'network_code' & value == layer_code)$layer),
                                                 type = 'host-parasite',
                                                 weight = interaction_yearx$weight,
                                                 method = 'literature_survey'))
      return(interactions)
    }
  }
 
}


countryx <- read_csv('afro_ken.csv')
afro_ang <- inter_table("afro_ang.csv")
afro_ken <- inter_table("afro_ken.csv")
afro_mad <- inter_table("afro_mad.csv")
afro_nam <- inter_table("afro_nam.csv")
afro_nig <- inter_table("afro_nig.csv")
afro_saf <- inter_table("afro_saf.csv")
afro_tan <- inter_table("afro_tan.csv")
afro_uga <- inter_table("afro_uga.csv")
afro_zai <- inter_table("afro_zai.csv")

interactions <- rbind(interactions, afro_ang,afro_ken,afro_mad,afro_nam,afro_nig,afro_saf,afro_tan,afro_uga,afro_zai)

nrct_cal <- inter_table("nrct_cal.csv")
nrct_con <- inter_table("nrct_con.csv")
nrct_flo <- inter_table("nrct_flo.csv")
nrct_ida <- inter_table("nrct_ida.csv")
nrct_ind <- inter_table("nrct_ind.csv")
nrct_man <- inter_table("nrct_man.csv")
nrct_mis <- inter_table("nrct_mis.csv")
nrct_mon <- inter_table("nrct_mon.csv")
nrct_nmx <- inter_table("nrct_nmx.csv")
nrct_ohi <- inter_table("nrct_ohi.csv")
nrct_ore <- inter_table("nrct_ore.csv")
nrct_ten <- inter_table("nrct_ten.csv")
nrct_tex <- inter_table("nrct_tex.csv")
nrct_uta <- inter_table("nrct_uta.csv")
nrct_wis <- inter_table("nrct_wis.csv")

interactions <- rbind(interactions, nrct_cal,nrct_con,nrct_flo,nrct_ida,nrct_ind,nrct_man,nrct_mis,nrct_mon,nrct_nmx,
                      nrct_ohi,nrct_ore,nrct_ten,nrct_tex,nrct_uta,nrct_wis)

ntrp_arn <- inter_table("ntrp_arn.csv")
ntrp_arp <- inter_table("ntrp_arp.csv")
ntrp_bra <- inter_table("ntrp_bra.csv")
ntrp_chi <- inter_table("ntrp_chi.csv")
ntrp_col <- inter_table("ntrp_col.csv")
ntrp_pan <- inter_table("ntrp_pan.csv")
ntrp_per <- inter_table("ntrp_per.csv")
ntrp_uru <- inter_table("ntrp_uru.csv")
ntrp_ven <- inter_table("ntrp_ven.csv")

interactions <- rbind(interactions, ntrp_arn,ntrp_arp,ntrp_bra,ntrp_chi,ntrp_col,ntrp_pan,ntrp_per,ntrp_uru,ntrp_ven)

plct_adz <- inter_table("plct_adz.csv")
plct_akm <- inter_table("plct_akm.csv")
plct_alt <- inter_table("plct_alt.csv")
plct_arm <- inter_table("plct_arm.csv")
plct_azb <- inter_table("plct_azb.csv")
plct_dal <- inter_table("plct_dal.csv")
plct_kos <- inter_table("plct_kos.csv")
plct_krj <- inter_table("plct_krj.csv")
plct_kur <- inter_table("plct_kur.csv")
plct_mon <- inter_table("plct_mon.csv")
plct_moy <- inter_table("plct_moy.csv")
plct_msc <- inter_table("plct_msc.csv")
plct_nsb <- inter_table("plct_nsb.csv")
plct_pol <- inter_table("plct_pol.csv")
plct_slo <- inter_table("plct_slo.csv")
plct_ttm <- inter_table("plct_ttm.csv")
plct_tur <- inter_table("plct_tur.csv")
plct_urm <- inter_table("plct_urm.csv")
plct_urv <- inter_table("plct_urv.csv")
plct_vgk <- inter_table("plct_vgk.csv")
plct_wsy <- inter_table("plct_wsy.csv")


interactions_test <- rbind(interactions, plct_adz,plct_akm,plct_alt,plct_arm,plct_azb,plct_dal,plct_kos,plct_krj,plct_kur,plct_mon
                      ,plct_moy,plct_msc,plct_nsb,plct_pol,plct_slo,plct_ttm,plct_tur,plct_urm,plct_urv,plct_vgk,plct_wsy)

#add a interaction id to each unique  interaction
interactions_test <- cbind(interaction_id = 1:nrow(interactions_test),interactions_test)
##transform the numeric types in the dataframe to character types so I can creatge a long format
interactions_test <- transform(interactions_test, layer_from = as.character(layer_from), 
          layer_to = as.character(layer_to), weight = as.character(weight)) 

#turn the wide format to a long format
interactions <- interactions_test %>%
  group_by(interaction_id) %>%
  pivot_longer(!interaction_id, names_to = 'attribute', values_to = 'value')


write_csv(interactions, 'interactions.csv')

#nodes
parasites <-as.vector(interactions_test$node_from)
hosts <- as.vector(interactions_test$node_to)
parasites <- unique(parasites)
hosts <- unique(hosts)

all_nodes <- c(parasites,hosts)

nodes <- tibble(node_id=1:(length(c(parasites$node_from ,hosts$node_to))),node_name=c(parasites$node_from,hosts$node_to),
                type=c(rep.int('Parasite',length(parasites$node_from)),rep.int('Host',length(hosts$node_to))))


nodes <- NULL
for (i in 1:length(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= c(rep.int('parasite',length(parasites)),rep.int('host',length(hosts)))[i]))

}

write_csv(nodes, 'nodes.csv')

#description
write_csv(create_description_csv(description_of_article = 'unpublished', source_of_data = 'owner_of_data',data_url = NA),'description.csv')


