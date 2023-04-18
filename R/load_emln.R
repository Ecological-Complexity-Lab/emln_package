#' Load networks
#'
#'@name load_emln
#'
#' @description Load specific networks via ID or network name, ID or name must be entred as list.

#'
#' @param network_id The network ID retrieved from the \code{emln_search} function.
#' @param ids Will the nodes and layers in the edge list returned are IDs or node names (default is IDs).
#'
#' @return An emln object containing 6 data frames of the relevant network - nodes, layers, extended edge list, state nodes, description, references. If one of the previous does not exist, their value will be NULL.
#' @return A multilayer object, which is an R list of dataframes:
#' \itemize{
#'   \item \code{nodes} Represents physical nodes' metadata. A node ID is assigned to each node
#'   \item \code{layers} All the details related to the layers that make up the multilayer network
#'   \item \code{extended} An extended edge list of the format \code{layer_from node_from layer_to node_to weight}. The interactions present in the data can contain both interlayer edges and intralayer edges
#'   \item \code{state_nodes} State node information
#'   \item \code{description} Contain a description of the created network.This information can be used to identify the creator of the data entry (Noa or Ofir) and the type of network (Spatial, Temporal, etc.) or ecological type (Seed-Dispersal, Pollination, etc.) it pertains to. Additionally, the entry mentions whether "state nodes" exist or not. A description line may provide additional information about the data and article. Furthermore, the entry also identifies the source of the data and provides a URL for accessing it
#'   \item \code{references} Contains all the relevant information required to find the article where the data is presented
#' }
#' @return The value of any missing data frames in the multilayer object will be set to NULL.
#'
#' @examples
#' # Load a network via network ID ( default ids is TRUE - In the result, the extended edge list is represented by node and layer IDs, not names)
#' load_emln(network_id = 2)
#'
#' # Load a network via network ID, without transforming the node and layer names to IDs in the extended edge list
#' load_emln(network_id = 55, ids = FALSE)
#' @export load_emln


load_emln <- function(network_id, ids = TRUE) {

  #get the network name in order to load it -- depending on the network id entered

    #network_name <- list.files('./data/',pattern = paste0('emln',as.character(network_id),'_'))
    pattern <- paste0("emln", network_id, "_")
    network_name <- descriptions %>% filter(str_detect(network_name, pattern)) %>% slice(1) %>% pull(network_name)
    if (length(network_name) == 0) {
      return(paste0('Error: Network id',network_name, ' does not exist'))
    } else {
      #network <- loadRData(paste0('./data/',network_name))
      network <- get(network_name)
    }

  #load the network and add to the big list depending on if it need to be wide or long

    #pivot nodes wider

    network$nodes <- network$nodes %>%
      tidyr::pivot_wider(names_from = 'attribute',values_from = 'value',
                           values_fn = list) %>%
      tidyr::unnest(cols = everything())

    #fix the column types (so not all columns are character type)

    for (i in colnames(network$nodes)) {network$nodes[[i]] <- hablar::retype(network$nodes[[i]])}

     #pivot layers wider

     network$layers <-  network$layers %>%
      tidyr::pivot_wider(names_from = 'attribute',
                         values_from = 'value', values_fn = list) %>%
       #if there are multiple types to the same layer, write them as individual rows and not lists
      tidyr::unnest(cols = everything()) %>%
       # The layer_name is necessary because it is used by functions like get_sam
       rename(layer_id=layer) %>%
       mutate(layer_name=paste('layer_',layer_id,sep=''))

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

     #pivot interactions wider

     network$interactions <-  network$interactions %>%
       tidyr::pivot_wider(names_from = 'attribute',
                          values_from = 'value', values_fn = list) %>%
       dplyr::select(interaction_id,layer_from,node_from,layer_to,node_to,weight) %>%
       tidyr::unnest(cols = everything()) %>%
       select(-interaction_id)

     #fix the column types

     for (i in colnames(network$interactions)) {network$interactions[[i]] <- hablar::retype(network$interactions[[i]])}


    #if there are state_nodes - pivot them wider
     if ('state_nodes' %in% names(network)) {
       network$state_nodes <- network$state_nodes %>%
         tidyr::pivot_wider(names_from = 'attribute',values_from = 'value',
                            values_fn = list) %>%
         tidyr::unnest(cols = everything())
     }

     #Shirly  - issue #70
     extended <- network$interactions
     extended <- extended[, !(names(extended) %in% "interaction_id")]
     #if (network$layers$name)
     if (("name" %in% colnames(network$layers)) && !(all(extended$layer_from %in% network$layers$name))) {
       # Replace values in 'layer_from' column of 'extended' with matching values from 'name' column of 'layers'
       extended$layer_from <- network$layers$name[match(extended$layer_from, network$layers$layer_id)]
       extended$layer_to <- network$layers$name[match(extended$layer_to, network$layers$layer_id)]
       }


    #turn the interactions node names to ids - if the user wants to (by default it does).

     if (ids == TRUE) {

      #join layer_from

       #if there is a "name" column in the layers.csv, and the layer_from is not numeric (not in ids)

       if (!all(unique(network$layers$layer_id) %in% unique(network$interactions$layer_from)) && ("name" %in% colnames(network$layers))) {
         if (!is.numeric(network$interactions$layer_from)) {
           network$interactions <- network$interactions %>%
             dplyr::left_join(y = network$layers[,c('layer_id','name')],
                              by = c('layer_from' = 'name')) %>%
             dplyr::select(-layer_from) %>%
             dplyr::rename(layer_from = layer_id)
         }
       }

       #if there isn't a "name" column in the layers.csv

      #join node_from

       if (!all(unique(network$nodes$node_id) %in% unique(network$interactions$node_from))) {
        network$interactions <- network$interactions %>%
          dplyr::left_join(y = network$nodes[, c('node_id','node_name')],
                           by = c("node_from" = "node_name")) %>%
          dplyr::select(-node_from) %>%
          dplyr::rename(node_from = node_id)
        }

      #join layer_to

       #if there is a "name" column in the layers.csv, and the layer_from is not numeric (not in ids)

       if (!all(unique(network$layers$layer_id) %in% unique(network$interactions$layer_to)) && ("name" %in% colnames(network$layers))) {
         if (!is.numeric(network$interactions$layer_to)) {
           network$interactions <- network$interactions %>%
             dplyr::left_join(y = network$layers[,c('layer_id','name')],
                              by = c('layer_to' = 'name')) %>%
             dplyr::select(-layer_to) %>%
             dplyr::rename(layer_to = layer_id)
         }
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
        dplyr::select(layer_from, node_from, layer_to, node_to, weight, tidyselect::everything())

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
           dplyr::select(layer_from, node_from, layer_to, node_to, weight, tidyselect::everything())
    }


    out <- list(nodes=network$nodes,
                layers=network$layers,
                extended=extended,
                extended_ids=network$interactions,
                state_nodes=network$state_nodes,
                description=network$description,
                references=network$references)

    class(out) <- 'multilayer'

    return(out)
}

