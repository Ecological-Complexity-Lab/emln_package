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


search_emln <- function(ecological_network_type=NULL,multilayer_network_type=NULL,state_nodes=NULL,node_number_cutoff=NULL,layer_number_cutoff=NULL, weighted = NULL, interlayer = NULL, directed =NULL) {
  result_tibble <- tibble()
  if (!is.null(ecological_network_type)) {
    result_tibble <- rbind(result_tibble,helper_attribute_ecomulti_type(ecological_network_type,result_tibble))
  }
  if (!is.null(multilayer_network_type)) {
    result_tibble <- rbind(result_tibble,helper_attribute_ecomulti_type(multilayer_network_type,result_tibble))
  }
  if (!is.null(state_nodes)) {
    result_tibble <- rbind(result_tibble,helper_attribute_state_nodes(state_nodes,result_tibble))
  }
  if (!is.null(node_number_cutoff)) {
    result_tibble <- rbind(result_tibble,helper_attribute_node_cutoff(node_number_cutoff,result_tibble))
  }
  if (!is.null(layer_number_cutoff)) {
    result_tibble <- rbind(result_tibble,helper_attribute_layer_cutoff(layer_number_cutoff,result_tibble))
  }
  if (!is.null(weighted)) {
    result_tibble <- rbind(result_tibble,helper_attribute_weighted(result_tibble))
  }
  if (!is.null(directed)) {
    result_tibble <- rbind(result_tibble,helper_attribute_directed(result_tibble))
  }
  if (!is.null(interlayer)) {
    result_tibble <- rbind(result_tibble,helper_attribute_interlayer(result_tibble))
  }
  return(result_tibble)
}

##############################################################################################################################

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}



helper_attribute_ecomulti_type <- function(attribute,result_tibble) {
    for (i in list.files("data")) {
      theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
      if(tolower(attribute) %in% tolower(filter(theloadedobjects$description, attribute == as.character(attribute))$value)) { #check if the current network[i] is of the searched type
        result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                     network_name =  gsub(i,pattern = '.rda',replacement = ''),
                                                     ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                     multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                     state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value,
                                                     weighted = if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE),
                                                     directed = if_else('directed' %in% theloadedobjects$layers$attribute, true = TRUE, false = FALSE),
                                                     interlayer = if_else('interlayer' %in% filter(theloadedobjects$interactions,attribute == 'type')$value, true = TRUE, false = FALSE)
        ))
        rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      } else {
        rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
        next
      }
    }
  return(result_tibble)
}

helper_attribute_node_cutoff <- function(attribute,result_tibble) {
  for (i in list.files("data")) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(max(theloadedobjects$nodes$node_id) >= attribute) { #check if the current network[i] is of the searched type
      result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                   network_name = gsub(i,pattern = '.rda',replacement = ''),
                                                   ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                   multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                   state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value,
                                                   weighted = if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE),
                                                   directed = if_else('directed' %in% theloadedobjects$layers$attribute, true = TRUE, false = FALSE),
                                                   interlayer = if_else('interlayer' %in% filter(theloadedobjects$interactions,attribute == 'type')$value, true = TRUE, false = FALSE)
      ))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(result_tibble)
}

helper_attribute_layer_cutoff <- function(attribute,result_tibble) {
  for (i in list.files("data")) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(max(theloadedobjects$layers$layer) >= attribute) { #check if the current network[i] is of the searched type
      result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                   network_name = gsub(i,pattern = '.rda',replacement = ''),
                                                   ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                   multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                   state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value,
                                                   weighted = if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE),
                                                   directed = if_else('directed' %in% theloadedobjects$layers$attribute, true = TRUE, false = FALSE),
                                                   interlayer = if_else('interlayer' %in% filter(theloadedobjects$interactions,attribute == 'type')$value, true = TRUE, false = FALSE)
      ))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(result_tibble)
}

helper_attribute_weighted <- function(result_tibble) {
  for (i in list.files("data")) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE)) { #check if the current network[i] is of the searched type
      result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                   network_name = gsub(i,pattern = '.rda',replacement = ''),
                                                   ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                   multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                   state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value,
                                                   weighted = if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE),
                                                   directed = if_else('directed' %in% theloadedobjects$layers$attribute, true = TRUE, false = FALSE),
                                                   interlayer = if_else('interlayer' %in% filter(theloadedobjects$interactions,attribute == 'type')$value, true = TRUE, false = FALSE)
      ))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(result_tibble)
}

helper_attribute_directed <- function(result_tibble) {
  for (i in list.files("data")) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(if_else('directed' %in% theloadedobjects$layers$attribute, true = TRUE, false = FALSE)) { #check if the current network[i] is of the searched type
      result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                   network_name = gsub(i,pattern = '.rda',replacement = ''),
                                                   ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                   multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                   state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value,
                                                   weighted = if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE),
                                                   directed = if_else('directed' %in% theloadedobjects$layers$attribute, true = TRUE, false = FALSE),
                                                   interlayer = if_else('interlayer' %in% filter(theloadedobjects$interactions,attribute == 'type')$value, true = TRUE, false = FALSE)
      ))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(result_tibble)
}


helper_attribute_interlayer <- function(result_tibble) {
  for (i in list.files("data")) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(if_else('interlayer' %in% filter(theloadedobjects$interactions,attribute == 'type')$value, true = TRUE, false = FALSE)) { #check if the current network[i] is of the searched type
      result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                   network_name = gsub(i,pattern = '.rda',replacement = ''),
                                                   ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                   multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                   state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value,
                                                   weighted = if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE),
                                                   directed = if_else('directed' %in% theloadedobjects$layers$attribute, true = TRUE, false = FALSE),
                                                   interlayer = if_else('interlayer' %in% filter(theloadedobjects$interactions,attribute == 'type')$value, true = TRUE, false = FALSE)
      ))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(result_tibble)
}

helper_attribute_state_nodes <- function(attribute,result_tibble) {
  for (i in list.files("data")) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(filter(theloadedobjects$description, attribute == 'state_nodes')$value == attribute) { #check if the current network[i] is of the searched type
      result_tibble <- rbind(result_tibble, tibble(network_id = as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')),
                                                   network_name = gsub(i,pattern = '.rda',replacement = ''),
                                                   ecological_network_type = filter(theloadedobjects$description, attribute == 'ecological_network_type')$value,
                                                   multilayer_network_type = filter(theloadedobjects$description, attribute == 'multilayer_network_type')$value,
                                                   state_nodes = filter(theloadedobjects$description, attribute == 'state_nodes')$value,
                                                   weighted = if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE),
                                                   directed = if_else('directed' %in% theloadedobjects$layers$attribute, true = TRUE, false = FALSE),
                                                   interlayer = if_else('interlayer' %in% filter(theloadedobjects$interactions,attribute == 'type')$value, true = TRUE, false = FALSE)
      ))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(result_tibble)
}

theloadedobjects <- loadRData('data/emln30_Spatial_Dupont_Olesen_2009.rda')

