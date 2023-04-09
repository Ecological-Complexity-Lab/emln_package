#' Convert a multilayer class to a supra-adjacency matrix
#'
#' This function creates a supra-adjacency matrix from a multilayer object.
#'
#' @param multilayer The multilayer object.
#' @param bipartite Is the network bipartite? Must be provided (no default value).
#' @param directed Is the network directed? Must be provided (no default value).
#' @param sparse Should the returned matrix be a sparse matrix (class Matrix)? Defaults to false.
#'
#' @return A matrix or sparse matrix object, and node mapping for easily linking row and column names to the node names.
#' @export
#'
#' @examples
#'

get_sam <- function(multilayer,bipartite,directed,sparse=F) {

  SAM_nodes_map <- NULL
  # supra_matrix <- muxViz::BuildSupraAdjacencyMatrixFromExtendedEdgelist(mEdges = as.data.frame(extended_edge_list),
  #                                                     Nodes =  nrow(nodes),
  #                                                     isDirected = Directed,
  #                                                     Layers = length(list_of_layers))

  if (!bipartite) {
    SAM_nodes_map <- bind_rows(
      tibble(layer_id = extended_edge_list$layer_from, node_id = extended_edge_list$node_from, state_node_id = 0),
      tibble(layer_id = extended_edge_list$layer_to, node_id = extended_edge_list$node_to, state_node_id = 0)) %>% unique()

    SAM_nodes_map <- SAM_nodes_map %>% left_join(nodes, by = c('node_id' = 'node_id'))
    SAM_nodes_map$state_node_id <- ifelse(SAM_nodes_map$layer_id ==1, SAM_nodes_map$node_id, (SAM_nodes_map$node_id+nrow(nodes)) )
    mEdges <- as.data.frame(extended_edge_list)
    Nodes <- nrow(nodes)
    layers <- length(list_of_layers)
    edges <- data.frame(from = mEdges[, 1] + Nodes * (mEdges[,2] - 1), to = mEdges[, 3] + Nodes * (mEdges[, 4] - 1),
                        weight = mEdges[, 5])
    M <- list_to_matrix(edges, bipartite = bipartite, directed = directed)$mat
  }
  if (bipartite){
    # Create an empty supra-adjacency matrix
    M <- matrix(0, nrow = 0, ncol = 0)
    # initialize an empty dataframe to hold the SAM_nodes_map
    SAM_nodes_map <- data.frame(layer_id = integer(),
                                node_id = integer(),
                                state_node_id = character(),
                                node_name = character())

    # create a named vector of node names
    node_names <- nodes$node_name
    names(node_names) <- nodes$node_id

    if (is_matrix == FALSE) {
      # Convert a list of intralayer edges to a list of matrices
      list_of_layers_T <-list()
      for (i in list_of_layers) {
        i <- (list_to_matrix(i, directed = directed, bipartite = TRUE))$mat
        list_of_layers_T <- c(list_of_layers_T, list(i))
      }
      list_of_layers <-list_of_layers_T
    }

    layer<-1

    for (i in list_of_layers)  {
      # Assign column and row names
      if (is.null(rownames(i))) {rownames(i) <- paste('R',1:nrow(i),sep='')}
      if (is.null(colnames(i))) {colnames(i) <- paste('C',1:ncol(i),sep='')}
      # Extract dimensions A and P from bipartite matrix
      P <- nrow(i)
      A <- ncol(i)
      # Transform bipartite matrix to a square matrix with dimensions N = A + P
      N <- A + P
      i_squ <- matrix(0, nrow = N, ncol = N, dimnames = list(c(rownames(i), colnames(i)), c(rownames(i), colnames(i))))
      i_squ[1:P, (P+1):N] <- i
      i_squ[(P+1):N, 1:P] <- t(i)

      # To populate SAM_nodes_map
      node_name <- rownames(i_squ)
      node_id <- match(node_name, node_names)

      # replace row and column names in M with state_node_id values
      rownames(i_squ) <- match(rownames(i_squ), node_names) + N*(layer-1)
      colnames(i_squ) <- match(colnames(i_squ), node_names) + N*(layer-1)

      # populate SAM_nodes_map
      state_node_id <- rownames(i_squ)
      new_SAM_nodes_map <- data.frame(layer_id = layer,
                                      node_id = node_id,
                                      state_node_id = state_node_id,
                                      node_name = node_name)

      SAM_nodes_map <- rbind(SAM_nodes_map, new_SAM_nodes_map)

      # Append the current matrix to the supra-adjacency matrix
      rownames_M <- c(rownames(M), rownames(i_squ))
      colnames_M <- c(colnames(M), colnames(i_squ))
      M <- rbind(cbind(M, matrix(0, nrow = ncol(M), ncol = nrow(i_squ))),
                 cbind(matrix(0, nrow = nrow(i_squ), ncol = ncol(M)), i_squ))
      rownames(M) <- rownames_M
      colnames(M) <- colnames_M

      layer <- layer+1

    }

    # Identify and populate interlayer interactions
    interlayer_edges <- extended_edge_list[extended_edge_list$layer_from != extended_edge_list$layer_to, ]

    for (i in seq_len(nrow(interlayer_edges))) {
      from_node <- interlayer_edges$node_from[i]
      from_layer <- interlayer_edges$layer_from[i]
      to_node <- interlayer_edges$node_to[i]
      to_layer <- interlayer_edges$layer_to[i]
      weight <- interlayer_edges$weight[i]
      from_state_node_id <- SAM_nodes_map[SAM_nodes_map$node_id == from_node & SAM_nodes_map$layer_id == from_layer, "state_node_id"]
      to_state_node_id <- SAM_nodes_map[SAM_nodes_map$node_id == to_node & SAM_nodes_map$layer_id == to_layer, "state_node_id"]
      M[to_state_node_id, from_state_node_id] <- weight
    }
  }


  return(list(SAM=M , SAM_nodes_map=SAM_nodes_map))}
}
