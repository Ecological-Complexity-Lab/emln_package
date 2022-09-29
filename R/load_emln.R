#' Load networks
#'
#'@name load_emln
#'
#' @description Load specific networks via id or network name, id or name must be entred as list.

#'
#' @param network_id The network ID retrieved from the emln_search function.
#' @param wide Will the layers, interactions and nodes return in a wide format (default is TRUE).
#' @param ids Will the nodes and layers in the edge list returned are ids or node names (default is ids).
#'
#' @return An emln object containing 5 dataframes of the relevant network - description, references, layer, nodes, state_nodes, interactions in a wide or long format. If one of the previous does not exist, their value will be NULL.
#'
#' @examples
#' Load a network via network id (default wide is TRUE, default ids is TRUE - the result is in wide format and edge list is with node and layer ids not names)
#' \code{load_emln(network_id = 15)}
#'
#' Load a network via network name in a long format without switching the node and layer names to ids in the edge list
#' \code{load_emln(network_id = 62, wide = FALSE, ids = FALSE)}
#' @import tidyr
#' @export load_emln


load_emln <- function(network_id, wide=TRUE, ids = TRUE) {

  #get the network name in order to load it -- depending on the network id entered

    network_name <- list.files('./data/',pattern = paste0('emln',as.character(network_id),'_'))
    if (length(network_name) == 0) {
      return(paste0('Error: Network id',network_name, ' does not exist'))
    } else {
      network <- loadRData(paste0('./data/',network_name))
    }

  #load the network and add to the big list depending on if it need to be wide or long

    #pivot nodes wider
    network$nodes <- network$nodes %>%
      tidyr::pivot_wider(names_from = 'attribute',values_from = 'value',
                           values_fn = list)
    #pivot interactions wider
    network$interactions <-  network$interactions %>%
      tidyr::pivot_wider(names_from = 'attribute',
                         values_from = 'value', values_fn = list) %>%
      dplyr::select(interaction_id,layer_from,node_from,layer_to,node_to,weight)
    #pivot layers wider
    network$layers <-  network$layers %>%
      tidyr::pivot_wider(names_from = 'attribute',
                         values_from = 'value', values_fn = list)


    #turn the interactions node names to ids - if the user wants to - default = it does
    #if statment is to make sure to do the conversion only if the layer/node from is not already in id format)
    if (ids == TRUE) {
      #join layer_from (
      if (!all(unique(network$layers$layer) %in% unique(network$interactions$layer_from))) {
          network$interactions <- network$interactions %>%
            dplyr::left_join(y = network$layers[,c('layer','layer_name')],
                             by = c('layer_from' = 'layer_name')) %>%
            dplyr::select(-layer_from) %>%
            dplyr::rename(layer_from = layer)
        }
      #join node_from
      if (!all(unique(network$nodes$node_id) %in% unique(network$interactions$node_from))) {
        network$interactions <- network$interactions %>%
          dplyr::left_join(y = network$nodes[, c('node_id','node_name')],
                           by = c("node_from" = "node_name")) %>%
          dplyr::select(-node_from) %>%
          dplyr::rename(node_from = node_id)

        }
      #join layer_to
      if (!all(unique(network$layers$layer) %in% unique(network$interactions$layer_to))) {
        network$interactions <- network$interactions %>%
          dplyr::left_join(y = network$layers[,c('layer','layer_name')],
                           by = c('layer_to' = 'layer_name')) %>%
          dplyr::select(-layer_to) %>%
          dplyr::rename(layer_to = layer)
      }
      #join node_to
      if (!all(unique(network$nodes$node_id) %in% unique(network$interactions$node_to))) {
        network$interactions <- network$interactions %>%
          dplyr::left_join(y = network$nodes[, c('node_id','node_name')],
                           by = c("node_to" = "node_name")) %>%
          dplyr::select(-node_to) %>%
          dplyr::rename(node_to = node_id)
      }

      #order the interactions

      network$interactions <- network$interactions %>%
        dplyr::select(interaction_id, layer_from, node_from, layer_to, node_to, weight, tidyselect::everything())
    }

    #turn the interactions layers and nodes to the infomapecology format

    intra <- network$interactions %>% dplyr::filter(as.data.frame(layer_from)==as.data.frame(layer_to))
    inter <- network$interactions %>% dplyr::filter(as.data.frame(layer_from)!=as.data.frame(layer_to))
    # If there are no interlayer edges
    if (nrow(inter)==0){inter <- NULL}

    out <- list(description=network$description,
                references=network$references,
                state_nodes=network$state_nodes,
                intra=intra,
                inter=inter,
                nodes=network$nodes,
                layers=network$layers)
    class(out) <- 'multilayer'
    return(out)
}


