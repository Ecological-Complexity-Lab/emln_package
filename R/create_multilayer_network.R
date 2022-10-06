#' Create Multilayer Network
#'
#' This function creates a multilayer network out of a list of matrices.
#'
#' @param list_of_matrices
#'
#' @return supra-adjacency matrix from a list of matrices
#' @export
#'
#' @examples
#' create_multilayer_network(list(bipartite::olesen2002aigrettes,bipartite::olesen2002flores), bipartite = TRUE, Directed = FALSE, interaction_type = 'pollination')

create_multilayer_network <- function(list_of_matrices, bipartite, Directed, interaction_type, isSupraMatrix) {

  nodes <- NULL
  extended_edge_list <- NULL

  #loop over the list of matrices, creating an edge list for each network seperetly and appending them to the original edge list
  for (i in list_of_matrices) {

    ###  edge list  ###
    if (bipartite) {igraph_network <- matrix_to_list_bipartite(x = i,group_names = c('Pollinator','Plants'))
                    edge_list <- igraph_network$edge_list}
    if (!bipartite) {igraph_network <- matrix_to_list_unipartite(x = i, directed = Directed)
                     edge_list <- igraph_network$edge_list}

    #add the current edge list to the extended_edge_list
    extended_edge_list <- rbind(extended_edge_list,tibble(node_from=edge_list$from, layer_from= layer, node_to=edge_list$to,
                                 layer_to = layer, weight=edge_list$weight,type=interaction_type))



    ###  nodes  ###
    nodes <- unique(c(nodes, igraph_network$nodes))
  }

  ###  conversion of the extended edge list to a supra-adjacency matrix  ###
  if (isSupraMatrix) {supra_matrix <- muxViz::BuildSupraAdjacencyMatrixFromExtendedEdgelist(mEdges = extended_edge_list,
                                                        Nodes =  nodes,
                                                        isDirected = Directed,
                                                        Layers = length(list_of_matrices))
  return(supra_matrix)}

  #if they dont want supra-adjacency matrix (??)
  return(list(extended_edge_list,nodes))

}
