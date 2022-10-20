#' Load networks
#'
#'@name load_emln
#'
#' @description Load specific networks via id or network name, id or name must be entred as list.

#'
#' @param network_id The network ID retrieved from the emln_search function.
#' @param ids Will the nodes and layers in the edge list returned are ids or node names (default is ids).
#'
#' @return An emln object containing 5 dataframes of the relevant network - description, references, layer, nodes, state_nodes, interactions. If one of the previous does not exist, their value will be NULL.
#'
#' @examples
#' Load a network via network id ( default ids is TRUE - the result is in wide format and edge list is with node and layer ids not names)
#' \code{load_emln(network_id = 15)}
#'
#' Load a network via network name in a long format without switching the node and layer names to ids in the edge list
#' \code{load_emln(network_id = 62, ids = FALSE)}
#' @export load_emln


load_emln <- function(network_id, ids = TRUE) {

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
                           values_fn = list) %>%
      tidyr::unnest(cols = everything())

    #fix the column types (so not all columns are character type)

    for (i in colnames(network$nodes)) {network$nodes[[i]] <- hablar::retype(network$nodes[[i]])}

    #pivot interactions wider

    network$interactions <-  network$interactions %>%
      tidyr::pivot_wider(names_from = 'attribute',
                         values_from = 'value', values_fn = list) %>%
      dplyr::select(interaction_id,layer_from,node_from,layer_to,node_to,weight) %>%
      tidyr::unnest(cols = everything())

    #fix the column types

    for (i in colnames(network$interactions)) {network$interactions[[i]] <- hablar::retype(network$interactions[[i]])}

    #pivot layers wider

     network$layers <-  network$layers %>%
      tidyr::pivot_wider(names_from = 'attribute',
                         values_from = 'value', values_fn = list) %>%
      tidyr::unnest(cols = everything()) #if there are multiple types to the same layer, write them as individual rows and not lists

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

    #turn the interactions node names to ids - if the user wants to - default = it does

     if (ids == TRUE) {

      #join layer_from

       if (!all(unique(network$layers$layer) %in% unique(network$interactions$layer_from))) {
          network$interactions <- network$interactions %>%
            dplyr::left_join(y = network$layers[,c('layer','name')],
                             by = c('layer_from' = 'name')) %>%
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
          dplyr::left_join(y = network$layers[,c('layer','name')],
                           by = c('layer_to' = 'name')) %>%
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
        dplyr::select(node_from, layer_from, node_to, layer_to, weight, tidyselect::everything(), -interaction_id)

      #convert the weight column to numeric from list
      #network$interactions$weight <- as.numeric(network$interactions$weight)

      #if the user doesn't want the ids to be used in the intra/inter formats, check if there are networks that *only* have ids

       } else {
      if (!'name' %in% colnames(network$layers)) {
          message('Warning: layer ids were used, no layer names were found.')
      }
      if (!'node_name' %in% colnames(network$nodes)) {
          message('Warning: node ids were used, no node names were found.')
      }

         #order the interactions

         network$interactions <- network$interactions %>%
           dplyr::select(node_from, layer_from, node_to, layer_to, weight, tidyselect::everything(), -interaction_id)
    }


    out <- list(nodes=network$nodes,
                layers=network$layers,
                extended=network$interactions,
                state_nodes=network$state_nodes,
                description=network$description,
                references=network$references)

    class(out) <- 'multilayer'

    return(out)
}

