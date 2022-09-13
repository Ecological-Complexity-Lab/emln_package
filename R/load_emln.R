#' Load networks
#'
#'@name load_emln
#'
#' @description Load specific network via id or network name

#'
#' @param network_id,network_name,wide=TRUE
#'
#' @return An emln object containing 5 dataframes of the relevant network - description, references (only if the network was obtained from a paper), layer, nodes, interactions in a or long format.
#'
#' @examples
#' \dontrun{
#' Load a network via network id (default wide is TRUE)
#' load_emln(network_id = 15)
#'
#' Load a network via network name in a long format
#' load_emln(network_name = 'emln20_spatial_angelini', wide = FALSE)
#' }
#' @export load_emln


load_emln <- function(network_id, network_name, wide=TRUE) {
  networks_wanted <- list()
  arguments_entered <- as.list(match.call())

    #get the network name in order to load it -- depending on the argument entered
  if ('network_id' %in% names(arguments_entered)) {
    for (i in arguments_entered$network_id) {
      network_name <- list.files('./data/',pattern = paste0('emln',as.character(i),'_'))
      if (length(network_name) == 0) {
        return(paste0('Error: Network id ',i, ' doesn\'t exist'))
      } else {
        networks_wanted <- c(networks_wanted,loadRData(paste0('./data/',network_name, '.rda')))
      }
    }
  } else if ('network_name' %in% names(arguments_entered)) {
    for (i in arguments_entered$network_name) {
      network_name <- list.files('./data/',pattern = as.character(i))
      if (length(network_name) == 0) {
        return(paste0('Error: Typo in network_name',i))
      } else {
        networks_wanted <- c(networks_wanted,loadRData(paste0('./data/',network_name, '.rda')))
      }
    }
  }

  #load the network and add to the big list depending on if it need to be wide or long
  if (wide == TRUE) {
    for (i in networks_wanted)
    #pivot nodes wider
    i$nodes <- pivot_wider(i$nodes, names_from = 'attribute',
                                 values_from = 'value', values_fn = list)
    #pivot interactions wider
    i$interactions <-  pivot_wider(i$interactions, names_from = 'attribute',
                                         values_from = 'value', values_fn = list)
    #pivot layers wider
    i$layers <-  pivot_wider(i$layers, names_from = 'attribute',
                                   values_from = 'value', values_fn = list)
        }
  return(networks_wanted)
}




load_emln(network_id = list(1,2))
