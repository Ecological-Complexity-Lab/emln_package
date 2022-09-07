#' Search parameters of datasets in package (search_emln)
#'
#' Get the network that has the parameters specified in the function
#' @param ecological_network_type
#' @param  multilayer_network_type
#' @param state_node
#' @param layer_number_cutoff
#' @param node_number_cutoff
#'
#' @return A tibble containing the network id, emln name and data url.
#' @return the object including all datasets corresponding to the query.
#' @examples
#' \dontrun{
#' Generates a tibble of all the networks in the package that are pollination networks
#' search_emln(ecological_network_type = 'pollination')
#'
#' Generates a tibble of all the networks in the package that have state nodes and are temporal
#' search_emln(multilayer_network_type = 'time', state_node = TRUE)
#' }
#' @export search_emln



search_emln <- function(ecological_network_type=NULL,multilayer_network_type=NULL,state_nodes=NULL,node_number_cutoff=NULL,layer_number_cutoff=NULL) {
  result_tibble <- tibble()
  if (!is.null(ecological_network_type)) {
    for (i in list.files("data")) {
      theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
      if(ecological_network_type %in% filter(theloadedobjects$description, attribute == 'ecological_network_type')$value) { #check if the current network[i] is of the searched type
        result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                                             network_name = i,
                                                                             ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                                             multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                                             state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value
                                                     ))
        rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      } else {
        rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
        next
      }
    }
  }
  if (!is.null(multilayer_network_type)) {
    for (i in list.files("data")) {
      theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
      if(filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value == multilayer_network_type) { #check if the current network[i] is of the searched type
        result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')), network_name = i,
                                                     ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                     multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                     state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value))
        rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      } else {
        rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
        next
      }
    }
  }
  if (!is.null(state_nodes)) {
      for (i in list.files("data")) {
        theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
        if(filter(theloadedobjects$description, attribute == 'state_nodes')$value == state_nodes) { #check if the current network[i] is of the searched type
          result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                       network_name = i,
                                                       ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                       multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                       state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value
                                                       ))
          rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
        } else {
          rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
          next
        }
      }
  }
        if (!is.null(node_number_cutoff)) {
            for (i in list.files("data")) {
              theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
              if(max(theloadedobjects$nodes$node_id) >= node_number_cutoff) { #check if the current network[i] is of the searched type
                result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                             network_name = i,
                                                             ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                             multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                             state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value
                                                             ))
                rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
              } else {
                rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
                next
              }
            }
        }
          if (!is.null(layer_number_cutoff)) {
              for (i in list.files("data")) {
                 theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
                if(max(theloadedobjects$layers$layer) >= layer_number_cutoff) { #check if the current network[i] is of the searched type
                  result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                               network_name = i,
                                                               ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                               multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                               state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value
                                                               ))
                  rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
                 } else {
                   rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
                   next
                }
    }
          }
  return(result_tibble)
}


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#
#
# check <- function(x=NULL,y=NULL) {
#   if (is.null(x)) {
#     return('hello')
#   } else if (is.null(y)) {
#     return('yes')
#   } else {
#     return('no')
#   }
# }
#

'pollination' %in% filter(theloadedobjects$description, attribute == 'ecological_network_type')$value

theloadedobjects <- loadRData('data/emln1_Environment_Schleuning_2010.rda')
