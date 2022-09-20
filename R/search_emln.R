#' Search networks
#'
#'@name search_emln
#'
#' @description Search the database for networks depeding on search parameters specified below.
#'
#' @details Possible Search Terms:
#' \describe{
#'   \item{\strong{multilayer_network_types}}{Environment/Temporal/Multiplex/Pertubation/Spatial}
#'   \item{\strong{ecological_network_types}}{Seed-Dispersal/Pollination/Food-Web/Host-Parasite/Multiples/Plant-Herbivore/Anemone-Fish/Plant-Ant}
#'   \item{\strong{state_nodes}}{TRUE/FALSE}
#'   \item{\strong{node_number_minimum}}{max node number is 3490}
#'   \item{\strong{layer_number_minimum}}{max layer number is 777}
#'   \item{\strong{weighted}}{TRUE/FALSE}
#'   \item{\strong{interlayer}}{TRUE/FALSE}
#'   \item{\strong{directed}}{TRUE/FALSE}
#' }
#'
#' @md
#'
#'
#' @param ecological_network_type,multilayer_network_type,state_node,layer_number_cutoff,node_number_cutoff,weighted,directed,interlayer
#'
#' @return A tibble containing the network id, and all the search arguments for each of the networks that are compatible with the search.
#'
#'
#' @examples
#' Generates a tibble of all the networks in the package that are pollination networks
#' \code{search_emln(ecological_network_type = 'Plant-Ant')}
#'
#' Generates a tibble of all the networks in the package that have state nodes and are temporal
#' \code{search_emln(multilayer_network_type = 'Temporal', state_node = TRUE)}
#'
#' @export search_emln

search_emln <- function(ecological_network_type=NULL,multilayer_network_type=NULL,
                        state_nodes=NULL,node_number_minimum=NULL,layer_number_minimum=NULL,
                        weighted = NULL, interlayer = NULL, directed =NULL){
  desc <- loadRData('./data/descriptions.rda') #load the large tibble that contains all the descriptions for it to be filtered
  attributes_entered <- compact(as.list(match.call()))[-1] #check which arguments were entered so i can loop over it.

  for (i in names(attributes_entered)) {
      if (i == 'ecological_network_type') {
        desc <- desc %>%
          dplyr::filter(ecological_network_type == stringr::str_to_title(attributes_entered[['ecological_network_type']]))
        next
      } else if (i == 'multilayer_network_type') {
        desc <- desc %>%
          dplyr::filter(multilayer_network_type == stringr::str_to_title(attributes_entered[['multilayer_network_type']]))
        next
      } else if (i == 'state_nodes') {
        desc <- desc %>%
          dplyr::filter(state_nodes == attributes_entered[['state_nodes']])
        next
      } else if (i == 'node_number_cutoff') {
        desc <- desc %>%
          dplyr::filter(node_number_cutoff >= attributes_entered[['node_number_cutoff']])
        next
      } else if (i == 'layer_number_cutoff') {
        desc <- desc %>%
          dplyr::filter(layer_number_cutoff >= attributes_entered[['layer_number_cutoff']])
        next
      } else if (i == 'weighted') {
        desc <- desc %>%
          dplyr::filter(weighted == attributes_entered[['weighted']])
        next
      } else if (i == 'directed') {
        desc <- desc %>%
          dplyr::filter(directed == attributes_entered[['directed']])
        next
      } else if (i == 'interlayer') {
        desc <- desc %>%
          dplyr::filter(interlayer == attributes_entered[['interlayer']])
        }
        next
      }
  # ids_to_keep <- Reduce(intersect, ids)
  # descriptions <- descriptions %>%
  #   filter(network_id %in% ids_to_keep) %>%
  #   group_by(network_id,network_name)  %>%
  #   arrange(network_id,network_name)
  desc <- desc %>%
    group_by(network_id,network_name)  %>%
    arrange(network_id,network_name)
  return(desc)
  }

