#' Search networks
#'
#'@name search_emln
#'
#' @description Search the database for networks depeding on search parameters specified below.
#'
#' @details
#' Possible Search Terms:
#> #' \tabular{cccccccc} {
#> #'   \strong{multilayer_network_types} \tab \strong{ecological_network_types} \tab \strong{state_nodes} \tab \strong{node_number_minimum} \tab \strong{layer_number_minimum} \tab \strong{weighted}\tab \strong{interlayer}\tab \strong{directed} \cr
#> #'   Environment/Temporal/Multiplex/Pertubation/Spatial \tab Seed-Dispersal/Pollination/Food-Web/Host-Parasite/Multiples/Plant-Herbivore/Anemone-Fish/Plant-Ant  \tab max node number is 3490 \tab max layer number is 777 \tab TRUE/FALSE \tab TRUE/FALSE \tab TRUE/FALSE \tab TRUE/FALSE \cr
#> }
#' \describe {
#'   \item{multilayer_network_types}{Environment/Temporal/Multiplex/Pertubation/Spatial}
#'   \item{ecological_network_types}{Seed-Dispersal/Pollination/Food-Web/Host-Parasite/Multiples/Plant-Herbivore/Anemone-Fish/Plant-Ant}
#'   \item{state_nodes}{TRUE/FALSE}
#'   \item{node_number_minimum}{TRUE/FALSE}
#'   \item{layer_number_minimum}{TRUE/FALSE}
#'   \item{weighted}{TRUE/FALSE}
#'   \item{interlayer}{TRUE/FALSE}
#'   \item{directed}{TRUE/FALSE}
#' }
#'
#' @md
#'
#'
#' @param ecological_network_type,multilayer_network_type,state_node,layer_number_cutoff,node_number_cutoff,weighted,directed,interlayer
#'
#' @return A tibble containing the network id, emln name and data url. The object including all datasets corresponding to the query.
#'
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
  desc <- loadRData('./data/descriptions.rda') #load the large tibble that contains all the descriptions for it to be filtered
  attributes_entered <- compact(as.list(match.call()))[-1] #check which arguments were entered so i can loop over it.

  for (i in names(attributes_entered)) {
      if (i == 'ecological_network_type') {
        desc <- desc %>%
          filter(ecological_network_type == str_to_title(attributes_entered[['ecological_network_type']]))
        next
      } else if (i == 'multilayer_network_type') {
        desc <- desc %>%
          filter(multilayer_network_type == str_to_title(attributes_entered[['multilayer_network_type']]))
        next
      } else if (i == 'state_nodes') {
        desc <- desc %>%
          filter(state_nodes == attributes_entered[['state_nodes']])
        next
      } else if (i == 'node_number_cutoff') {
        desc <- desc %>%
          filter(node_number_cutoff >= attributes_entered[['node_number_cutoff']])
        next
      } else if (i == 'layer_number_cutoff') {
        desc <- desc %>%
          filter(layer_number_cutoff >= attributes_entered[['layer_number_cutoff']])
        next
      } else if (i == 'weighted') {
        desc <- desc %>%
          filter(weighted == attributes_entered[['weighted']])
        next
      } else if (i == 'directed') {
        desc <- desc %>%
          filter(directed == attributes_entered[['directed']])
        next
      } else if (i == 'interlayer') {
        desc <- desc %>%
          filter(interlayer == attributes_entered[['interlayer']])
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





tabular::tabular(search)
