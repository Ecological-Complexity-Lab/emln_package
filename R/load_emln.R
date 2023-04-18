#' Load networks include din the package
#'
#' @name load_emln
#'
#' @description Load a specific network by specifying its ID.
#'
#' @param network_id The network ID.
#'
#' @return A multilayer object.
#'
#' @details
#' Use the \code{search_emln} and \code{view_emln} functions to obtain network IDs.
#'
#' @examples
#'
#' load_emln(network_id = 38)
#'
#' @export load_emln


load_emln <- function(network_id) {
  #get the network name in order to load it -- depending on the network id entered
  #FOR DEBUG: network_name <- list.files('./data/',pattern = paste0('emln',as.character(network_id),'_'))
  pattern <- paste0("emln", network_id, "_")
  network_name <- descriptions %>% filter(str_detect(network_name, pattern)) %>% slice(1) %>% pull(network_name)
  if (length(network_name) == 0) {
    return(paste0('Error: Network id',network_name, ' does not exist'))
  } else {
    #FOR DEBUG: network <- loadRData(paste0('./data/',network_name))
    network <- get(network_name)
  }

  #Pivot nodes to a wide format
  network$nodes %<>%
    tidyr::pivot_wider(names_from = 'attribute',values_from = 'value', values_fn = list) %>%
    tidyr::unnest(cols = everything())
    #fix the column types (so not all columns are character type)
    for (i in colnames(network$nodes)) {network$nodes[[i]] <- hablar::retype(network$nodes[[i]])}

  #Pivot layers to a wide format
  network$layers %<>%
    tidyr::pivot_wider(names_from = 'attribute', values_from = 'value', values_fn = list) %>%
    #if there are multiple types to the same layer, write them as individual rows and not lists
    tidyr::unnest(cols = everything()) %>%
    # The layer_id and layer_name are necessary because they are used by functions like get_sam
    rename(layer_id=layer)
    if ('name' %in% names(network$layers)) {
      network$layers %<>% rename(layer_name=name)
    } else {
      network$layers %<>% mutate(layer_name=paste('layer_',layer_id,sep=''))
    }

  #fix the column types
  for (i in colnames(network$layers)) {
    if('date' %in% tolower(i)) {
      if(all(grepl(pattern = '/', x = network$layers$date))) { #if there are dates that are written with '/' (wrong format to fix)
        network$layers$date <- strptime(as.character(network$layers$date), "%d/%m/%Y") #fix the format
        format(network$layers$date, "%Y-%m-%d")
      }
    }
    network$layers[[i]] <- hablar::retype(network$layers[[i]])
  }

  #Pivot links to a wide format
  network$interactions %<>%
    tidyr::pivot_wider(names_from = 'attribute', values_from = 'value', values_fn = list) %>%
     # dplyr::select(interaction_id,layer_from,node_from,layer_to,node_to,weight) %>%
     tidyr::unnest(cols = everything()) %>%
     select(-interaction_id) %>%
    select(layer_from, node_from, layer_to, node_to, everything())
  #fix the column types
  for (i in colnames(network$interactions)) {network$interactions[[i]] <- hablar::retype(network$interactions[[i]])}

  # If the layers are ids, change them to layer_name
  if (is.numeric(network$interactions$layer_from)) {network$interactions$layer_from <- paste('layer_',network$interactions$layer_from,sep='')}
  if (is.numeric(network$interactions$layer_to)) {network$interactions$layer_to <- paste('layer_',network$interactions$layer_to,sep='')}

  # Create a map of state nodes
  print('Creating state node map')
  state_nodes_map <-
  bind_rows(network$interactions %>% distinct(layer_name=layer_from, node_name=node_from),
            network$interactions %>% distinct(layer_name=layer_to, node_name=node_to)) %>%
    distinct() %>%
    left_join(network$nodes, by='node_name') %>%
    left_join(network$layers %>% select(layer_id, layer_name), by='layer_name') %>%
    select(layer_id, node_id, layer_name, node_name, everything())

  #Pivot state nodes to a wide format if more data on state nodes exist
  if ('state_nodes' %in% names(network)) {
  network$state_nodes %<>%
      tidyr::pivot_wider(names_from = 'attribute',values_from = 'value', values_fn = list) %>%
      tidyr::unnest(cols = everything())
  print('Joinign with state node information in the data set')
  state_nodes_map %<>% left_join(network$state_nodes %>% select(-node_name), by=c('layer_id', 'node_id'))
  }

  ###  conversion of the node names to node ids in the edge list  ###
  print('Creating extended link list with node IDs')
  extended_edge_list <- extended_edge_list_ids <- as.data.frame(network$interactions)
  for (i in 1:nrow(extended_edge_list)) {
    extended_edge_list_ids[i,2] <- network$nodes[network$nodes$node_name == extended_edge_list[i,2],]$node_id
    extended_edge_list_ids[i,4] <- network$nodes[network$nodes$node_name == extended_edge_list[i,4],]$node_id
    # Change layer names for ids
    extended_edge_list_ids[i,1] <- network$layers[network$layers$layer_name == extended_edge_list[i,1],]$layer_id
    extended_edge_list_ids[i,3] <- network$layers[network$layers$layer_name == extended_edge_list[i,3],]$layer_id
  }

  out <- list(nodes=network$nodes,
              layers=network$layers,
              extended=extended_edge_list,
              extended_ids=extended_edge_list_ids,
              state_nodes=state_nodes_map,
              description=network$description,
              references=network$references)
  class(out) <- 'multilayer'

  return(out)
}

