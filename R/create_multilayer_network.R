#' Create Multilayer Network
#'
#' This function creates a multilayer network out of a list of matrices / list of intralayer edges.
#'
#' @param list_of_layers List of matrices or list of interlayer edges.
#' @param bipartite Is the network bipartite? Must be provided (no default value).
#' @param directed Is the network directed? Must be provided (no default value).
#' @param interlayer_links Interlayer extended edge list of the format \code{layer_from node_from layer_to node_to weight}. Specify layer and nodes by names. Layer names should correspond to those provided in layer_attributes. Default is NULL, assuming no interlayer links.
#' @param layer_attributes Optional. A data frame with layer attributes. The first column must be \code{layer_id} and the second \code{layer_name}.  The order of the rows and layer_id should be the same as in \code{list_of_layers}.
#' @param state_nodes_user Optional. Additional information on physical nodes in layers. Must contain columns \code{layer_name} and \code{node_name}
#' @param get_sam Should supra-adjacency matrix be returned?
#'
#' @return A multilayer object, which is an R list of dataframes:
#' \itemize{
#'   \item \code{nodes} Nodes' metadata
#'   \item \code{layers} Layers's metadata
#'   \item \code{extended} An extended edge list of the format \code{layer_from node_from layer_to node_to weight}. All nodes and layers are identified by unique IDs rather than names
#'   \item \code{state_nodes} State node information
#'   \item \code{description} Contain a description of the created network
#'   \item \code{references} Contain reference details
#' }
#' @return The value of any missing data frames in the multilayer object will be set to NULL.
#' @return The supra-adjacency matrix (SAM) and node mapping (SAM_nodes_map), if it was requested.
#' @export
#'
#' @examples
#'

create_multilayer_network <- function(list_of_layers,
                                      bipartite,
                                      directed,
                                      interlayer_links = NULL,
                                      layer_attributes = NULL,
                                      state_nodes_user = NULL,
                                      get_sam=F) {


  extended_edge_list <- NULL
  SAM_nodes_map <- NULL
  state_nodes <- NULL

  # Check validity of layer attribute table
  if (is.null(layer_attributes)) {
    print('Layer attributes not provided, I added them (see layer_attributes in the final object)')
    layer_attributes <- data.frame(layer_id = 1:length(list_of_layers), layer_name=paste('layer_',1:length(list_of_layers),sep = ''))
  } else {
    layer_attributes <- data.frame(layer_attributes) # working with data frame is easier than with tibbles.
    if (names(layer_attributes)[1]!='layer_id') {stop('First column in layer attributes should be layer_id, and the order should be as provied in the list of layers.')}
    if (names(layer_attributes)[2]!='layer_name') {stop('Second column in layer attributes should be layer_name.')}
  }

  #Loop over the list of layers, creating an edge list for each network separately and appending them to the original edge list.
  for (layer_id in 1:length(list_of_layers)) {
    # get the layer network
    the_layer <- list_of_layers[[layer_id]]

    print(sprintf('Layer #%s processing.', layer_id))
    ###  edge list  ###
    if (bipartite) {igraph_network <- create_monolayer_object(x = the_layer, directed = directed, bipartite = T, group_names = c('set_cols','set_rows'))
                    edge_list <- igraph_network$edge_list
                    print('Done.')}
    if (!bipartite) {
                     #Expected a symmetric matrix in an undirected network. Otherwise, throw the warning.
                     if ((directed == FALSE) && (isSymmetric(the_layer) ==FALSE)){
                       warning('WARNING: In an undirected network a symmetric matrix is expected. Proceed with caution!')
                     }
                     igraph_network <- create_monolayer_object(x = the_layer, directed = directed, bipartite = F)
                     edge_list <- igraph_network$edge_list
                     print('Done.')
                     }

    #add the current edge list to the extended_edge_list
    extended_edge_list <- rbind(extended_edge_list,
                                data.frame(layer_from = layer_attributes[layer_id,'layer_name'],
                                                          node_from=edge_list$from,
                                                          layer_to = layer_attributes[layer_id,'layer_name'],
                                                          node_to=edge_list$to,
                                                          weight=edge_list$weight))


    ###  state nodes  ###
    state_nodes <- rbind(state_nodes, data.frame(layer_id=layer_id, node_name=igraph_network$nodes$node_name))
  }

  # If interlayer_links exists - do an rbind to the extended edge list
  if (!is.null(interlayer_links)){
    # If some nodes do not appear in the list of provided layers, interlayer_links will not be added to the extended edge list
    nodes_in_interlayer <- unique(c(interlayer_links$node_from, interlayer_links$node_to))
    nodes_in_layers <- unique(c(extended_edge_list$node_from, extended_edge_list$node_to))
    if (any(nodes_in_interlayer %in% nodes_in_layers)==F){
      stop('STOPPING: Check node names in the interlayer_links dataframe. Some nodes do not appear in the list of provided layers')
    }
    # If some layers do not appear in the list of provided layers, interlayer_links will not be added to the extended edge list
    layers_in_interlayer <- unique(c(interlayer_links$layer_from, interlayer_links$layer_to))
    layers_in_intralayer <- unique(c(extended_edge_list$layer_from, extended_edge_list$layer_to))
    if (any(layers_in_interlayer %in% layers_in_intralayer)==F){
      stop('STOPPING: Check layer names in the interlayer_links dataframe. Some layers do not appear in the list of provided layer attributes. You must provide a layer attributes table that include layer names.')
    }

    # Add the interlayer links
    extended_edge_list <- rbind(extended_edge_list, interlayer_links)
  }


  # Create node ids
  physical_nodes <- unique(state_nodes$node_name)
  physical_nodes <- data.frame(node_id = 1:length(physical_nodes), node_name = sort(physical_nodes))

  ###  conversion of the node names to node ids in the edge list  ###
  print('Creating extended link list with node IDs')
  extended_edge_list_ids <- extended_edge_list
  for (i in 1:nrow(extended_edge_list)) {
    extended_edge_list_ids[i,2] <- physical_nodes[physical_nodes$node_name == extended_edge_list[i,2],]$node_id
    extended_edge_list_ids[i,4] <- physical_nodes[physical_nodes$node_name == extended_edge_list[i,4],]$node_id
    # Change layer names for ids
    extended_edge_list_ids[i,1] <- layer_attributes[layer_attributes$layer_name == extended_edge_list[i,1],]$layer_id
    extended_edge_list_ids[i,3] <- layer_attributes[layer_attributes$layer_name == extended_edge_list[i,3],]$layer_id
  }
  # Make IDs integer instead of character
  extended_edge_list_ids[1:4,] <- apply(extended_edge_list_ids[1:4,], MARGIN = 2, FUN = as.integer)


  # Organized the layer_attributes and state_nodes
  print('Organizing state nodes')
  state_nodes <- as_tibble(state_nodes)
  suppressMessages(
  state_nodes %<>% left_join(layer_attributes) %>%
    left_join(as_tibble(physical_nodes)) %>%
    select(layer_id, node_id, layer_name, node_name)
  )
  if (!is.null(state_nodes_user)){
    print('Joining with user-provided state nodes')
    state_nodes %<>% left_join(state_nodes_user)
  }

  ###  conversion of the extended edge list to a supra-adjacency matrix  ###
  if (get_sam) {
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

  #M <- (M + Matrix::t(M)) # Make matrix symmetric
  out <- list(nodes=nodes,
              #layers = select(bind_cols(tibble(layer = 1:length(list_of_layers), directed = directed),layer_attributes), layer, colnames(layersInput), directed ),
              layers = bind_cols(tibble(layer = 1:length(list_of_layers), directed = directed),layer_attributes),
              extended=extended_edge_list,
              state_nodes=as_tibble(state_nodes),
              description=NULL,
              references=NULL)
  class(out) <- 'multilayer'
  return(list(Multilayer=out, SAM=M , SAM_nodes_map=SAM_nodes_map))}

  ###  if they don't want supra-adjacency matrix  ###
  out <- list(nodes=as_tibble(physical_nodes),
              extended=as_tibble(extended_edge_list),
              extended_ids=as_tibble(extended_edge_list_ids),
              layer_attributes = as_tibble(layer_attributes),
              state_nodes=as_tibble(state_nodes),
              description=NULL,
              references=NULL)

  class(out) <- 'multilayer'

  return(out)
}


### examples ###

# x <- matrix(rbinom(n = 15, size = 1, prob = 0.5),nrow=3, ncol = 5)
# y <- matrix(rbinom(n = 15, size = 1, prob = 0.5),nrow=3, ncol = 5)
#
# matrix1 <- as.matrix(create_multilayer_network(list(x,y), bipartite = T, directed = T,
#                                                get_sam = T))
# matrix2 <- as.matrix(create_multilayer_network(list(bipartite::olesen2002aigrettes,bipartite::olesen2002flores),
#                                                get_sam = T, bipartite = T, directed = F, interlayer_links = data.frame(node_from = 'Protaetia.aurichalcea', layer_from = 1, node_to = 'Reseda:luteola', layer_to = 2, weight = 234)))
#
#
#
#
# image(x = matrix2, useRaster=TRUE, axes=F)


#### QA ### Shirly
#
# x_binary_Nonames_square<-matrix(rbinom(16, 1, 0.6), 4, 4)
# x_Nobinary_Nonames_square <- x_binary_Nonames_square*round(runif(16,1,4),0)
#
# x_Nobinary_names_square<-x_Nobinary_Nonames_square
# rownames(x_Nobinary_names_square)<-c('Orange', 'Blue', 'Red', 'Green')
# colnames(x_Nobinary_names_square)<-c('Orange', 'Blue', 'Red', 'Green')
#
# x2_Nobinary_names_square<-x_Nobinary_Nonames_square
# rownames(x2_Nobinary_names_square)<-c('Red', 'Purple', 'Green', 'White')
# colnames(x2_Nobinary_names_square)<-c('Red', 'Purple', 'Green', 'White')
#
# x3_Nobinary_names_square<-x_Nobinary_Nonames_square
# rownames(x3_Nobinary_names_square)<-c('Orange', 'Purple', 'Red', 'White')
# colnames(x3_Nobinary_names_square)<-c('Orange', 'Purple', 'Red', 'White')
#
#
# xy_Nobinary_namesColRow_square<-x_Nobinary_names_square
# colnames(xy_Nobinary_namesColRow_square) <-c("Xx", "Gg","Aa","Bb")

