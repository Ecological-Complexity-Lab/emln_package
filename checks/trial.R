library(igraph)



site_interactions_files <- list.files(path = '~/GitHub/emln_db/EMLN_Datasets/noa_frydman/Spatial_Temporal_Martins2020',pattern = 'site\\d+_period\\d+.csv',full.names = TRUE)
Sierras_intralayer <-NULL
for (l in site_interactions_files) {
  x <- read_csv('l')
  # Convert to an edge list as we saw before
  g <- graph.incidence(t(x), directed = F, weighted = T) # transposing ensures that from is at the columns and to is the rows
  # Create the edge list for the layer
  l_bip <- as_tibble(igraph::as_data_frame(g, 'edges')) %>% 
    rename(node_from=from, node_to=to) %>% 
    mutate(layer_from=l, layer_to=l) %>% 
    dplyr::select(layer_from, node_from,layer_to, node_to, weight)
}
