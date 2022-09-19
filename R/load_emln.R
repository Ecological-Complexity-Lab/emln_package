#' Load networks
#'
#'@name load_emln
#'
#' @description Load specific networks via id or network name, id or name must be entred as list.

#'
#' @param network_id,,wide=TRUE
#'
#' @return An emln object containing 5 dataframes of the relevant network - description, references (only if the network was obtained from a paper), layer, nodes, interactions in a or long format.
#'
#' @examples
#' \dontrun{
#' Load a network via network id (default wide is TRUE)
#' load_emln(network_id = list(15))
#'
#' Load a network via network name in a long format
#' load_emln(network_name = list('emln20_spatial_angelini'), wide = FALSE)
#' }
#' @export load_emln


load_emln <- function(network_id = NULL, network_name = NULL, wide=TRUE) {
  networks_wanted <- list()
  arguments_entered <- as.list(match.call())

    #get the network name in order to load it -- depending on the argument entered
  if (!is.null(network_id)) {
    for (i in unique(as.list(network_id))) {
      network_name <- list.files('./data/',pattern = paste0('emln',as.character(i),'_'))
      if (length(network_name) == 0) {
        return(paste0('Error: Network id ',i, ' does not exist'))
      } else {
        networks_wanted[[network_name]] <- loadRData(paste0('./data/',network_name))
      }
    }
  }

  #load the network and add to the big list depending on if it need to be wide or long
  if (wide == TRUE) {
    for (i in names(networks_wanted)) {
    #pivot nodes wider
      networks_wanted[[i]]$nodes <- pivot_wider(networks_wanted[[i]]$nodes, names_from = 'attribute',
                                 values_from = 'value', values_fn = list)
    #pivot interactions wider
    networks_wanted[[i]]$interactions <-  pivot_wider(networks_wanted[[i]]$interactions, names_from = 'attribute',
                                         values_from = 'value', values_fn = list)
    #pivot layers wider
    networks_wanted[[i]]$layers <-  pivot_wider(networks_wanted[[i]]$layers, names_from = 'attribute',
                                   values_from = 'value', values_fn = list)
  }
}
  return(networks_wanted)
}

