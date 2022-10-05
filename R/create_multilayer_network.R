#' Create Multilayer Network
#'
#' This function creates a multilayer network out of a list of matrices.
#'
#' @param list_of_matrices
#'
#' @return
#' @export
#'
#' @examples

create_multilayer_network <- function(list_of_matrices) {
  nodes <- NULL
  extended_edge_list <- NULL

  #create a tibble of nodes
  for (i in list_of_matrices) {}
  countryx <- read_csv(file_name)
  countryx<-column_to_rownames(countryx,var = colnames(countryx)[1])
  view(countryx)
  interaction_yearx <- matrix_to_list_bipartite(countryx,group_names = c('Pollinator','Plants'))$edge_list
  interaction_yearx <- tibble(node_from=interaction_yearx$from, layer_from= layer, node_to=interaction_yearx$to,
                              layer_to = layer, weight=interaction_yearx$weight,type='pollination', method = 'observation')

  #a multilayer extended edge list



  #conversion of the extended edge list to a supra-adjacency matrix

  #muxViz::BuildSupraAdjacencyMatrixFromExtendedEdgelist()
}
