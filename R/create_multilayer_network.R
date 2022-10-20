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
#' create_multilayer_network(list(bipartite::olesen2002aigrettes,bipartite::olesen2002flores), bipartite = TRUE, Directed = FALSE, isSupraMatrix = TRUE)

create_multilayer_network <- function(list_of_matrices, bipartite, Directed, isSupraMatrix, Interlayer = NULL) {

  node_names <- NULL
  extended_edge_list <- NULL
  layer <- 0


  #loop over the list of matrices, creating an edge list for each network seperetly and appending them to the original edge list
  for (i in list_of_matrices) {
    layer <- layer+1
    ###  edge list  ###
    if (bipartite) {igraph_network <- matrix_to_list_bipartite(x = i,group_names = c('set_cols','set_rows'))
                    edge_list <- igraph_network$edge_list}
    if (!bipartite) {igraph_network <- matrix_to_list_unipartite(x = i, directed = Directed)
                     edge_list <- igraph_network$edge_list}

    #add the current edge list to the extended_edge_list
    extended_edge_list <- rbind(extended_edge_list,tibble(node_from=edge_list$from, layer_from= layer, node_to=edge_list$to,
                                 layer_to = layer, weight=edge_list$weight))


    ###  nodes  ###
    node_names <- unique(c(node_names,igraph_network$nodes$node_name))

  }


  ##if inter exists - do an rbind to the extended edge list
  if (!is.null(Interlayer)) {extended_edge_list <- rbind(extended_edge_list,Interlayer)}


  #Create node ids
  nodes <- tibble(node_id = 1:length(node_names), node_name = node_names)


  ###  conversion of the nodes names to node ids in the edge list  ###
  for (i in 1:nrow(extended_edge_list)) {
    extended_edge_list[i,1] <- as.character(nodes[nodes$node_name == unlist(extended_edge_list[i,1]),]$node_id)
    extended_edge_list[i,3] <- as.character(nodes[nodes$node_name == unlist(extended_edge_list[i,3]),]$node_id)
  }

  extended_edge_list <- hablar::retype(extended_edge_list)


  ###  conversion of the extended edge list to a supra-adjacency matrix  ###
  if (isSupraMatrix) {
    # supra_matrix <- muxViz::BuildSupraAdjacencyMatrixFromExtendedEdgelist(mEdges = as.data.frame(extended_edge_list),
    #                                                     Nodes =  nrow(nodes),
    #                                                     isDirected = Directed,
    #                                                     Layers = length(list_of_matrices))
  mEdges <- as.data.frame(extended_edge_list)
  Nodes <- nrow(nodes)
  layers <- length(list_of_matrices)
  edges <- data.frame(from = mEdges[, 1] + Nodes * (mEdges[,2] - 1), to = mEdges[, 3] + Nodes * (mEdges[, 4] - 1),
                      weight = mEdges[, 5])
  M <- list_to_matrix(edges, bipartite = bipartite, directed = Directed)$mat
  #M <- (M + Matrix::t(M)) # Make matrix symmetric
  return(M)}

  #if they don't want supra-adjacency matrix
  out <- list(nodes=nodes,
              layers=NULL,
              extended=extended_edge_list,
              state_nodes=NULL,
              description=NULL,
              references=NULL)

  class(out) <- 'multilayer'

  return(out)
}



# x <- matrix(rbinom(n = 15, size = 1, prob = 1),nrow=3, ncol = 5)
# y <- matrix(rbinom(n = 15, size = 1, prob = 1),nrow=3, ncol = 5)
#
# matrix1 <- as.matrix(create_multilayer_network(list(x,y), bipartite = T, Directed = T,
#                                                isSupraMatrix = T))
# matrix2 <- as.matrix(create_multilayer_network(list(bipartite::olesen2002aigrettes,bipartite::olesen2002flores),
#                                                isSupraMatrix = T, bipartite = T, Directed = F, Interlayer = data.frame(node_from = 'Protaetia.aurichalcea', layer_from = 1, node_to = 'Reseda:luteola', layer_to = 2, weight = 234)))
#
#
#
#
# image(x = matrix2, useRaster=TRUE, axes=F)



