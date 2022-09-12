#' Search networks
#'
#'@name search_emln
#'
#' @description Search the database for networks depeding on search parameters specified below.
#'
#' @details
#' Possible Search Terms:
#'
#' |multilayer_network_types                           |ecological_network_types                                                                           |state_nodes |node_number_minimum     |layer_number_minimum    |weighted   |interlayer |directed   |
#' |:--------------------------------------------------:|:--------------------------------------------------------------------------------------------------:|:-----------:|:-----------------------:|:-----------------------:|:----------:|:----------:|:----------:|
#' |Environment/Temporal/Multiplex/Pertubation/Spatial |Seed-Dispersal/Pollination/Food-Web/Host-Parasite/Multiples/Plant-Herbivore/Anemone-Fish/Plant-Ant |TRUE/FALSE  |max node number is 3490 |max layer number is 777 |TRUE/FALSE |TRUE/FALSE |TRUE/FALSE |
#'
#' @md
#'
#'
#' @param ecological_network_type,multilayer_network_type,state_node,layer_number_cutoff,node_number_cutoff,weighted,directed,interlayer
#'
#' @return A tibble containing the network id, emln name and data url. The object including all datasets corresponding to the query.
#'
#' @examples
#' \dontrun{
#' Generates a tibble of all the networks in the package that are pollination networks
#' search_emln(ecological_network_type = 'pollination')
#'
#' Generates a tibble of all the networks in the package that have state nodes and are temporal
#' search_emln(multilayer_network_type = 'time', state_node = TRUE)
#' }
#' @export search_emln


search_emln <- function(ecological_network_type=NULL,multilayer_network_type=NULL,
                        state_nodes=NULL,node_number_minimum=NULL,layer_number_minimum=NULL,
                        weighted = NULL, interlayer = NULL, directed =NULL){
  load('./data/descriptions.rda') #load the large tibble that contains all the descriptions for it to be filtered
  attributes_entered <- compact(as.list(match.call()))[-1] #check which arguments were entered so i can loop over it.
  ##create empty vectors that will contain the ids that answer the arguments demands, later they will be intersected

  ids <- list()

  for (i in names(attributes_entered)) {
      if (i == 'ecological_network_type') {
        ids$ids1 <- c(ids$ids1,helper_attribute_ecomulti_type(ecological_network_type,ids$ids1))
        next
      } else if (i == 'multilayer_network_type') {
        ids$ids2 <- c(ids$ids2,helper_attribute_ecomulti_type(multilayer_network_type,ids$ids2))
        next
      } else if (i == 'state_nodes') {
        ids$ids3 <- c(ids$ids3,helper_attribute_state_nodes(state_nodes,ids$ids3))
        next
      } else if (i == 'node_number_cutoff') {
        ids$ids4 <- c(ids$ids4,helper_attribute_node_cutoff(node_number_cutoff,ids$ids4))
        next
      } else if (i == 'layer_number_cutoff') {
        ids$ids5 <- c(ids$ids5,helper_attribute_layer_cutoff(layer_number_cutoff,ids$ids5))
        next
      } else if (i == 'weighted') {
        ids$ids6 <- c(ids$ids6,helper_attribute_weighted(ids$ids6))
        next
      } else if (i == 'directed') {
        ids$ids7 <- c(ids$ids7,helper_attribute_directed(ids$ids7))
        next
      } else if (i == 'interlayer') {
        ids$ids8 <- c(ids$ids8,helper_attribute_interlayer(ids$ids8))
        next
      }
    }
  ids_to_keep <- Reduce(intersect, ids)
  descriptions <- descriptions %>%
    filter(network_id %in% ids_to_keep)
  return(descriptions)
  }


# Helper Functions --------------------------------------------------------


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

helper_attribute_ecomulti_type <- function(attribute_temp,ids) {
    for (i in list.files("data")[-1]) {
      theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
      if(tolower(attribute_temp) %in% tolower(theloadedobjects$description$value)) { #check if the current network[i] is of the searched type
        ids <- c(ids, as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')))
        rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      } else {
        rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
        next
      }
    }
  return(ids)
}

helper_attribute_node_cutoff <- function(attribute,ids) {
  for (i in list.files("data")[-1]) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(max(theloadedobjects$nodes$node_id) >= attribute) { #check if the current network[i] is of the searched type
      ids <- c(ids, as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(ids)
}

helper_attribute_layer_cutoff <- function(attribute,ids) {
  for (i in list.files("data")[-1]) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(max(theloadedobjects$layers$layer) >= attribute) { #check if the current network[i] is of the searched type
      ids <- c(ids, as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(ids)
}

helper_attribute_weighted <- function(ids) {
  for (i in list.files("data")[-1]) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE)) { #check if the current network[i] is of the searched type
      ids <- c(ids, as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(ids)
}

helper_attribute_directed <- function(ids) {
  for (i in list.files("data")[-1]) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(if_else('directed' %in% theloadedobjects$layers$attribute, true = TRUE, false = FALSE)) { #check if the current network[i] is of the searched type
      ids <- c(ids, as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(ids)
}


helper_attribute_interlayer <- function(ids) {
  for (i in list.files("data")[-1]) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(if_else('interlayer' %in% filter(theloadedobjects$interactions,attribute == 'type')$value, true = TRUE, false = FALSE)) { #check if the current network[i] is of the searched type
      ids <- c(ids, as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(ids)
}

helper_attribute_state_nodes <- function(attribute,ids) {
  for (i in list.files("data")[-1]) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(filter(theloadedobjects$description, attribute == 'state_nodes')$value == attribute) { #check if the current network[i] is of the searched type
      ids <- c(ids, as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(ids)
}

