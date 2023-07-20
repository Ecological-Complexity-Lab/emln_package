library(readr)


# 20 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/20_Spatial_Angelini')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')


inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)


all(inter_nodes%in%node_nodes)  ###TRUE = FIXED



# 21 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/21_Spatial_Angelini_2013')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

all(inter_nodes%in%node_nodes)  ###TRUE = FIXED



# 24 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/24_Spatial_Canadian-fish-parasite')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

nodes$node_id <- trimws(nodes$node_id)
nodes <- as.data.frame(apply(nodes,c(1,2),gsub,pattern=' ',replacement='_'))
interactions$interaction_id <- trimws(interactions$interaction_id)
interactions <- as.data.frame(apply(interactions,c(1,2),gsub,pattern=' ',replacement='_'))

write_csv(interactions,'interactions.csv')
write_csv(nodes,'nodes.csv')

all(inter_nodes%in%node_nodes)  ###TRUE = FIXED


# 34 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/34_Spatial_Hadfield_2013/')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

nodes$node_id <- trimws(nodes$node_id)
nodes <- as.data.frame(apply(nodes,c(1,2),gsub,pattern=' ',replacement='_'))
interactions$interaction_id <- trimws(interactions$interaction_id)
interactions <- as.data.frame(apply(interactions,c(1,2),gsub,pattern=' ',replacement='_'))

#what nodes in interactions arent in nodes
inter_nodes[!inter_nodes%in%node_nodes]
#fix the dots in the middle - change them to '_'
interactions$value <- gsub(interactions$value, pattern = '\\.',replacement = '_')


write_csv(interactions,'interactions.csv')
write_csv(nodes,'nodes.csv')

all(inter_nodes%in%node_nodes)  ###TRUE = FIXED


# 43 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/43_Spatial_Melian_2015/')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

nodes$node_id <- trimws(nodes$node_id)
nodes <- as.data.frame(apply(nodes,c(1,2),gsub,pattern=' ',replacement='_'))
interactions$interaction_id <- trimws(interactions$interaction_id)
interactions <- as.data.frame(apply(interactions,c(1,2),gsub,pattern=' ',replacement='_'))

#what nodes in interactions arent in nodes
inter_nodes[!inter_nodes%in%node_nodes]
#fix 1 occurrence
nodes$value <- gsub(nodes$value, pattern = 'P._mindariphagum', replacement = 'P.mindariphagum')


write_csv(interactions,'interactions.csv')
write_csv(nodes,'nodes.csv')

all(inter_nodes%in%node_nodes)  ###TRUE = FIXED




# 45 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/45_Spatial_Montero_2005/')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

all(inter_nodes%in%node_nodes)  ###TRUE = FIXED

#######doesnt show any problems!


# 6 -----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/6_Environment_Kohler_2011/')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

#what nodes in interactions arent in nodes
inter_nodes[!inter_nodes%in%node_nodes]
#fix
nodes$node_id <- trimws(nodes$node_id)
nodes <- as.data.frame(apply(nodes,c(1,2),gsub,pattern=' ',replacement='_'))
interactions$interaction_id <- trimws(interactions$interaction_id)
interactions <- as.data.frame(apply(interactions,c(1,2),gsub,pattern=' ',replacement='_'))


write_csv(interactions,'interactions.csv')
write_csv(nodes,'nodes.csv')

all(inter_nodes%in%node_nodes)  ###TRUE = FIXED



# 61 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/61_Spatial_Temporal_Martins_2020/')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

#what nodes in interactions arent in nodes
inter_nodes[!inter_nodes%in%node_nodes]
#fix
###the names in the list are different from the interactions - ill use the interactions for the nodes
herb <- unique(interactions[interactions$attribute == 'node_from',]$value)
plant <-unique(interactions[interactions$attribute =='node_to',]$value)
all_nodes <- c(herb,plant)
nodes <- NULL
for (i in 1:length(all_nodes)) {
  nodes <- rbind(nodes,tibble(node_id=i,attribute='node_name',value=all_nodes[i]))
  nodes <- rbind(nodes,tibble(node_id=i,attribute='type',value= c(rep.int('herbivore',length(herb)),rep.int('plant',length(plant)))[i]))
}

all(inter_nodes%in%node_nodes)  ###TRUE = FIXED

write_csv(interactions,'interactions.csv')
write_csv(nodes,'nodes.csv')



# 62 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/62_Spatial_Thompson_2003/')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

#what nodes in interactions arent in nodes
inter_nodes[!inter_nodes%in%node_nodes]

all(inter_nodes%in%node_nodes)  ###TRUE = FIXED




# 63 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/63_Spatial_Trojelsgaard_2015/')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

#what nodes in interactions arent in nodes
inter_nodes[!inter_nodes%in%node_nodes]

#fix
nodes$value <- gsub(nodes$value, pattern = '[[:punct:]]|//.',replacement = '_')
nodes$value <- gsub(nodes$value, pattern = '[[:punct:]]{2,}',replacement = '_')
nodes$value <- gsub(nodes$value, pattern = '[[:punct:]]$',replacement = '')

interactions$value <- gsub(interactions$value, pattern = '//.$',replacement = '')
interactions$value <- gsub(interactions$value, pattern = '[[:punct:]]',replacement = '_')
interactions$value <- gsub(interactions$value, pattern = '[[:punct:]]{2,}',replacement = '')
interactions$value <- gsub(interactions$value, pattern = '\xfc\xbe\x8d\x93\xa0\xbc',replacement = '_85')



all(inter_nodes%in%node_nodes)  ###TRUE = FIXED


write_csv(nodes, 'nodes.csv')
write_csv(interactions, 'interactions.csv')



# 74 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/74_Temporal_Perturbation_Kaiser-Bunbury_2009/')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

#what nodes in interactions arent in nodes
inter_nodes[!inter_nodes%in%node_nodes]

all(inter_nodes%in%node_nodes)  ###TRUE = FIXED


###this network seems ok



# 76 ----------------------------------------------------------------------
setwd('~/GitHub/emln_db/EMLN_Datasets/datasets/76_Temporal_Ramos_robles_2016/')
interactions <- read_csv("interactions.csv")
nodes <- read_csv('nodes.csv')

inter_nodes <- unique(interactions[interactions$attribute %in% c('node_from','node_to'),]$value)
node_nodes <-unique(nodes[nodes$attribute == 'node_name',]$value)

length(inter_nodes)
length(node_nodes)

#what nodes in interactions arent in nodes
inter_nodes[!inter_nodes%in%node_nodes]


#fix
#as that node doesnt exist other than in the interactions, I will delete its interactions.
interactions <- interactions %>%
  tidyr::pivot_wider(names_from = 'attribute',values_from = 'value',
                     values_fn = list) %>%
  tidyr::unnest(cols = everything())

interactions <- interactions[!interactions$node_to == 'B43',]
interactions$interaction_id <- 1:nrow(interactions)

interactions <- interactions %>%
  tidyr::pivot_longer(!interaction_id, names_to = 'attribute',values_to = 'value') %>%
  tidyr::unnest(cols = everything())


all(inter_nodes%in%node_nodes)  ###TRUE = FIXED

write_csv(interactions, 'interactions.csv')






# add the taxa verification for the nodes that lost it due to the  --------

taxa_ver <- c(21,61,62)
#21
nodes_wo_ver <- read_csv('~/GitHub/emln_db/EMLN_Datasets/datasets/21_Spatial_Angelini_2013/nodes.csv')
nodes_w_ver <- read_csv('~/GitHub/emln_db/EMLN_Datasets/datasets copy/21_Spatial_Angelini_2013/nodes.csv')

for(i in nodes_wo_ver$node_id) {
  nodes_wo_ver <- rbind(nodes_wo_ver, nodes_w_ver[nodes_w_ver$node_id == i & (nodes_w_ver$attribute == 'taxa_verified'| nodes_w_ver$attribute == 'verification_level'),])
}

nodes_wo_ver <- nodes_wo_ver %>%
  arrange(node_id) %>%
  distinct()


write_csv(nodes_wo_ver, '~/GitHub/emln_db/EMLN_Datasets/datasets/21_Spatial_Angelini_2013/nodes.csv')

#61
nodes_wo_ver <- read_csv('~/GitHub/emln_db/EMLN_Datasets/datasets/61_Spatial_Temporal_Martins_2020/nodes.csv')
nodes_w_ver <- read_csv('~/GitHub/emln_db/EMLN_Datasets/datasets copy/61_Spatial_Temporal_Martins_2020/nodes.csv')

for(i in nodes_wo_ver$node_id) {
  nodes_wo_ver <- rbind(nodes_wo_ver, nodes_w_ver[nodes_w_ver$node_id == i & (nodes_w_ver$attribute == 'taxa_verified'| nodes_w_ver$attribute == 'verification_level'),])
}

nodes_wo_ver <- nodes_wo_ver %>%
  arrange(node_id) %>%
  distinct()

write_csv(nodes_wo_ver, '~/GitHub/emln_db/EMLN_Datasets/datasets/61_Spatial_Temporal_Martins_2020/nodes.csv')


#62
nodes_wo_ver <- read_csv('~/GitHub/emln_db/EMLN_Datasets/datasets/62_Spatial_Thompson_2003/nodes.csv')
nodes_w_ver <- read_csv('~/GitHub/emln_db/EMLN_Datasets/datasets copy/62_Spatial_Thompson_2003/nodes.csv')

for(i in nodes_wo_ver$node_id) {
  nodes_wo_ver <- rbind(nodes_wo_ver, nodes_w_ver[nodes_w_ver$node_id == i & (nodes_w_ver$attribute == 'taxa_verified'| nodes_w_ver$attribute == 'verification_level'),])
}

nodes_wo_ver <- nodes_wo_ver %>%
  arrange(node_id) %>%
  distinct()

write_csv(nodes_wo_ver, '~/GitHub/emln_db/EMLN_Datasets/datasets/62_Spatial_Thompson_2003/nodes.csv')




# check the fix w/ Shirley's code-----------------------------------------------------------

# create a list of all folder names in the main directory
# the path to the directory containing the 81 folders
folder_names <- list.dirs(path = "~/GitHub/emln_package/data-raw", recursive = FALSE)

# initialize an empty list to store the results
results <- list()

# loop through each folder in the list
# file1 = interactions.csv
# file2 = nodes.csv
for (folder_name in folder_names) {

  # construct the file paths for file1.csv and file2.csv
  file1_path <- file.path(folder_name, "interactions.csv")
  file2_path <- file.path(folder_name, "nodes.csv")

  # read the files into data frames
  file1_df <- read.csv(file1_path)
  file2_df <- read.csv(file2_path)

  # extract the values for node_from, node_to, and node_name
  node_from <- file1_df$value[file1_df$attribute == "node_from"]
  node_to <- file1_df$value[file1_df$attribute == "node_to"]
  node_name <- file2_df$value[file2_df$attribute == "node_name"]

  # check if node_from and node_to are in node_name
  if (!(all(node_from %in% node_name) && all(node_to %in% node_name))) {

    # store the folder name and the corresponding node names that didn't match
    invalid_nodes <- c(setdiff(node_from, node_name), setdiff(node_to, node_name))
    results[[folder_name]] <- invalid_nodes
  }
}

