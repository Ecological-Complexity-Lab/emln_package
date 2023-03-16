#' Create Multilayer Network
#'
#' This function creates a multilayer network out of a list of matrices.
#'
#' @param list_of_matrices List of matrices
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

create_multilayer_network <- function(list_of_matrices, isMatrix = TRUE, bipartite, Directed, isSupraMatrix, Interlayer = NULL) {

  node_names <- NULL
  extended_edge_list <- NULL
  node_names_layer <-NULL
  layer <- 0
  SAM_nodes_map <- NULL

  if (isMatrix == TRUE){

    #Loop over the list of matrices, creating an edge list for each network separately and appending them to the original edge list.
    for (i in list_of_matrices) {
      layer <- layer+1
      ###  edge list  ###
      if (bipartite) {print(sprintf('Layer # %s processing.', layer))
                      #Future development: Supra-adjacency matrix for the bipartite network.
                      if (isSupraMatrix == TRUE)
                          stop('ERROR: supra-adjacency matrix is suitable only for unipartite networks')
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
  #M <- (M + Matrix::t(M)) # Make matrix symmetric
  out <- list(nodes=nodes,
              layers = tibble(layer = 1:length(list_of_matrices), directed = Directed),
              extended=extended_edge_list,
              state_nodes=NULL,
              description=NULL,
              references=NULL)
  class(out) <- 'multilayer'
  return(list(Multilayer=out, SAM=M , SAM_nodes_map=SAM_nodes_map))}

  ###  if they don't want supra-adjacency matrix  ###
  out <- list(nodes=nodes,
              layers = tibble(layer = 1:length(list_of_matrices), directed = Directed),
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

