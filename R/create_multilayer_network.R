#' Create Multilayer Network
#'
#' This function creates a multilayer network out of a list of matrices / list of intralayer edges.
#'
#' @param list_of_matrices List of matrices or list of interlayer edges.
#' @param isMatrix By default, the expected input format is a list of matrices. If you have a list of intralayer edges instead, set 'isMatrix' as FALSE.
#' @param bipartite Is the network bipartite?
#' @param Directed Is the network directed?
#' @param isSupraMatrix Should supra-adjacency matrix be returned?
#' @param Interlayer Interlayer extended edge list of the format \code{node_from layer_from node_to layer_to weight}. Specify layers by IDs,  and nodes by names. Default is NULL, assuming a lack of interlayer edges.
#'
#' @return A multilayer object, which is an R list of dataframes:
#' \itemize{
#'   \item \code{nodes} Nodes' metadata
#'   \item \code{layers} Layers's metadata
#'   \item \code{extended} An extended edge list of the format \code{node_from layer_from node_to layer_to weight}. All nodes and layers are identified by unique IDs rather than names
#'   \item \code{state_nodes} State node information
#'   \item \code{description} Contain a description of the created network
#'   \item \code{references} Contain reference details
#' }
#' @return The value of any missing data frames in the multilayer object will be set to NULL.
#' @return The supra-adjacency matrix (SAM) and node mapping (SAM_nodes_map), if it was requested.
#' @export
#'
#' @examples
#' create_multilayer_network(list(bipartite::olesen2002aigrettes,bipartite::olesen2002flores), bipartite = TRUE, Directed = FALSE, isSupraMatrix = FALSE)

create_multilayer_network <- function(list_of_matrices, isMatrix = TRUE, layersData = NULL, bipartite, Directed, isSupraMatrix, Interlayer = NULL) {

  node_names <- NULL
  extended_edge_list <- NULL
  node_names_layer <-NULL
  layer <- 0
  SAM_nodes_map <- NULL

  if (is.null(layersData)) {layersData<- tibble(layerNumber = 1:length(list_of_matrices))}

  if (isMatrix == TRUE){

    #Loop over the list of matrices, creating an edge list for each network separately and appending them to the original edge list.
    for (i in list_of_matrices) {
      layer <- layer+1
      ###  edge list  ###
      if (bipartite) {print(sprintf('Layer # %s processing.', layer))
                      igraph_network <- matrix_to_list_bipartite(x = i,group_names = c('set_cols','set_rows'))
                      edge_list <- igraph_network$edge_list
                      print('Done.')}
      if (!bipartite) {print(sprintf('Layer # %s processing.', layer))
                       #Expected a symmetric matrix in an undirected network. Otherwise, throw the warning.
                       if ((Directed == FALSE) && (isSymmetric(i) ==FALSE))
                              print(sprintf('WARNING: In an undirected network expected a symmetric matrix. Proceed with caution! The result edge list is unexpected.'))
                       igraph_network <- matrix_to_list_unipartite(x = i, directed = Directed)
                       edge_list <- igraph_network$edge_list
                       print('Done.')}

      #add the current edge list to the extended_edge_list
      extended_edge_list <- rbind(extended_edge_list,tibble(node_from=edge_list$from, layer_from= layer, node_to=edge_list$to,
                                   layer_to = layer, weight=edge_list$weight))


      ###  nodes  ###
      node_names <- unique(c(node_names,igraph_network$nodes$node_name))
      node_names_layer <- rbind(node_names_layer, tibble(node_number = 1:length(igraph_network$nodes$node_name), node_name = igraph_network$nodes$node_name, node_layer = layer))

    }
  }

  else{

    for (i in list_of_matrices) {
      layer <- layer+1
      print(sprintf('Layer # %s processing.', layer))
      edge_list <- i
      extended_edge_list <- rbind(extended_edge_list,tibble(node_from=as.character(edge_list$from), layer_from= layer, node_to=as.character(edge_list$to),
                                                            layer_to = layer, weight=edge_list$weight))
      ###  nodes  ###

      node_names <- unique(c(node_names,i$from))
      node_names <- unique(c(node_names,i$to))
      node_names_layer <- rbind(node_names_layer, tibble(node_number = 1:length(node_names), node_name = node_names, node_layer = layer))
      print('Done.')
    }
  }

  # If Interlayer exists - do an rbind to the extended edge list
  # If some nodes do not appear in the list of provided matrices, Interlayer will not be added to the extended edge list.
  if (!is.null(Interlayer)) {

                            tInter_from <- Interlayer %>% select(node_from, layer_from) %>% unique()
                            tInter_to <- Interlayer %>% select(node_to, layer_to) %>% unique()
                            tNodes <- node_names_layer %>% select(node_name, node_layer) %>% unique()

                            colnames(tInter_from) <- colnames(tInter_to) <- colnames(tNodes)

                            tInter_all <- bind_rows(tInter_from, tInter_to) %>% unique()
                            tMerge <-merge(tNodes, tInter_all)


                            if (dim(tMerge)[1]==dim(tInter_all)[1])
                                                extended_edge_list <- rbind(extended_edge_list,Interlayer)
                            else{
                                  tAntiMerge <- anti_join(tInter_all, tNodes)
                                  print('WARNING: Check node names in the Interlayer dataframe. Some nodes do not appear in the list of provided matrices.')
                                  print(sprintf('Problematic nodes : %s', list(tAntiMerge)))
                                  print('Interlayer not added.')}
  }


  # Create node ids
  nodes <- tibble(node_id = 1:length(node_names), node_name = node_names)


  ###  conversion of the nodes names to node ids in the edge list  ###
  for (i in 1:nrow(extended_edge_list)) {
    extended_edge_list[i,1] <- as.character(nodes[nodes$node_name == unlist(extended_edge_list[i,1]),]$node_id)
    extended_edge_list[i,3] <- as.character(nodes[nodes$node_name == unlist(extended_edge_list[i,3]),]$node_id)
  }

  extended_edge_list <- hablar::retype(extended_edge_list) ###?




  ###  conversion of the extended edge list to a supra-adjacency matrix  ###
  if (isSupraMatrix) {
    # supra_matrix <- muxViz::BuildSupraAdjacencyMatrixFromExtendedEdgelist(mEdges = as.data.frame(extended_edge_list),
    #                                                     Nodes =  nrow(nodes),
    #                                                     isDirected = Directed,
    #                                                     Layers = length(list_of_matrices))

    if (!bipartite) {
        SAM_nodes_map <- bind_rows(
          tibble(layer_id = extended_edge_list$layer_from, node_id = extended_edge_list$node_from, state_node_id = 0),
          tibble(layer_id = extended_edge_list$layer_to, node_id = extended_edge_list$node_to, state_node_id = 0)) %>% unique()

        SAM_nodes_map <- SAM_nodes_map %>% left_join(nodes, by = c('node_id' = 'node_id'))
        SAM_nodes_map$state_node_id <- ifelse(SAM_nodes_map$layer_id ==1, SAM_nodes_map$node_id, (SAM_nodes_map$node_id+nrow(nodes)) )
        mEdges <- as.data.frame(extended_edge_list)
        Nodes <- nrow(nodes)
        layers <- length(list_of_matrices)
        edges <- data.frame(from = mEdges[, 1] + Nodes * (mEdges[,2] - 1), to = mEdges[, 3] + Nodes * (mEdges[, 4] - 1),
                            weight = mEdges[, 5])
        M <- list_to_matrix(edges, bipartite = bipartite, directed = Directed)$mat
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

      if (isMatrix == FALSE) {
        # Convert a list of intralayer edges to a list of matrices
        list_of_matrices_T <-list()
        for (i in list_of_matrices) {
              i <- (list_to_matrix(i, directed = Directed, bipartite = TRUE))$mat
              list_of_matrices_T <- c(list_of_matrices_T, list(i))
      }
        list_of_matrices <-list_of_matrices_T
      }

      layer<-1

      for (i in list_of_matrices)  {
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
              #layers = select(bind_cols(tibble(layer = 1:length(list_of_matrices), directed = Directed),layersData), layer, colnames(layersInput), directed ),
              layers = bind_cols(tibble(layer = 1:length(list_of_matrices), directed = Directed),layersData),
              extended=extended_edge_list,
              state_nodes=NULL,
              description=NULL,
              references=NULL)
  class(out) <- 'multilayer'
  return(list(Multilayer=out, SAM=M , SAM_nodes_map=SAM_nodes_map))}

  ###  if they don't want supra-adjacency matrix  ###
  out <- list(nodes=nodes,
              #layers = select(bind_cols(tibble(layer = 1:length(list_of_matrices), directed = Directed),layersData), layer, colnames(layersInput), directed ),
              layers = bind_cols(tibble(layer = 1:length(list_of_matrices), directed = Directed),layersData),
              extended=extended_edge_list,
              state_nodes=NULL,
              description=NULL,
              references=NULL)

  class(out) <- 'multilayer'

  return(out)
}


### examples ###

# x <- matrix(rbinom(n = 15, size = 1, prob = 0.5),nrow=3, ncol = 5)
# y <- matrix(rbinom(n = 15, size = 1, prob = 0.5),nrow=3, ncol = 5)
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


#### QA ### Shirly

x_binary_Nonames_square<-matrix(rbinom(16, 1, 0.6), 4, 4)
x_Nobinary_Nonames_square <- x_binary_Nonames_square*round(runif(16,1,4),0)

x_Nobinary_names_square<-x_Nobinary_Nonames_square
rownames(x_Nobinary_names_square)<-c('Orange', 'Blue', 'Red', 'Green')
colnames(x_Nobinary_names_square)<-c('Orange', 'Blue', 'Red', 'Green')

x2_Nobinary_names_square<-x_Nobinary_Nonames_square
rownames(x2_Nobinary_names_square)<-c('Red', 'Purple', 'Green', 'White')
colnames(x2_Nobinary_names_square)<-c('Red', 'Purple', 'Green', 'White')

x3_Nobinary_names_square<-x_Nobinary_Nonames_square
rownames(x3_Nobinary_names_square)<-c('Orange', 'Purple', 'Red', 'White')
colnames(x3_Nobinary_names_square)<-c('Orange', 'Purple', 'Red', 'White')


xy_Nobinary_namesColRow_square<-x_Nobinary_names_square
colnames(xy_Nobinary_namesColRow_square) <-c("Xx", "Gg","Aa","Bb")

