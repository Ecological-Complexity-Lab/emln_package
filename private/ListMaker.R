library(ggplot2)
library(readr)
library(tibble)
library(dplyr)
library(readr)
library(stringr)


# In this r file, I move all the formats of each network from the  --------


#read the file path
parent.folder <- "~/GitHub/emln_package/datasets"

setwd("~/GitHub/emln_package/datasets") ##in this chunk, we want the working directory to be the one with all the networks in order to read them

sub.folders.short <- list.dirs(path = parent.folder, recursive = T, full.names = F)[-1]
sub.folders.long <- list.dirs(path = parent.folder, recursive = T, full.names = T)[-1]

#order the sub folders according to the id, using the short sub folders so I can use the id, but I eventually need the long to read the csvs
sub.folders.long <- sub.folders.long[str_order(sub.folders.short, numeric = T)]


for (i in 1:length(sub.folders.long)) {
  if (file.exists(paste(sub.folders.long[[i]],'references.csv',sep = '/'))) {
    if (file.exists(paste(sub.folders.long[[i]],'state_nodes.csv',sep = '/'))) {
      network <- list(description = read_csv(paste(sub.folders.long[[i]],'description.csv',sep = '/')),
                      references = read_csv(paste(sub.folders.long[[i]],'references.csv',sep = '/')),
                      nodes = read_csv(paste(sub.folders.long[[i]],'nodes.csv',sep = '/')),
                      state_nodes = read_csv(paste(sub.folders.long[[i]],'state_nodes.csv',sep = '/')),
                      interactions = read_csv(paste(sub.folders.long[[i]],'interactions.csv',sep = '/')))
      class(network) <- 'EMLN'
      saveRDS( file=paste('/Users/noni/Documents/DataLists/',sub.folders.short[i],'.Rdata',sep=''), network)
    } else {
      network <- list(description = read_csv(paste(sub.folders.long[[i]],'description.csv',sep = '/')),
                      references = read_csv(paste(sub.folders.long[[i]],'references.csv',sep = '/')),
                      nodes = read_csv(paste(sub.folders.long[[i]],'nodes.csv',sep = '/')),
                      interactions = read_csv(paste(sub.folders.long[[i]],'interactions.csv',sep = '/')))
      class(network) <- 'EMLN'
      saveRDS(file=paste('/Users/noni/Documents/DataLists/',sub.folders.short[i],'.Rdata',sep=''), network)
    }
  } else {
    if (file.exists(paste(sub.folders.long[[i]],'state_nodes.csv',sep = '/'))) {
      network <- list(description = read_csv(paste(sub.folders.long[[i]],'description.csv',sep = '/')),
                      nodes = read_csv(paste(sub.folders.long[[i]],'nodes.csv',sep = '/')),
                      state_nodes = read_csv(paste(sub.folders.long[[i]],'state_nodes.csv',sep = '/')),
                      interactions = read_csv(paste(sub.folders.long[[i]],'interactions.csv',sep = '/')))
      class(network) <- 'EMLN'
      saveRDS( file=paste('/Users/noni/Documents/DataLists/',sub.folders.short[i],'.Rdata',sep=''), network)
    } else {
      network <- list(description = read_csv(paste(sub.folders.long[[i]],'description.csv',sep = '/')),
                      nodes = read_csv(paste(sub.folders.long[[i]],'nodes.csv',sep = '/')),
                      interactions = read_csv(paste(sub.folders.long[[i]],'interactions.csv',sep = '/')))
      class(network) <- 'EMLN'
      saveRDS(file=paste('/Users/noni/Documents/DataLists/',sub.folders.short[i],'.Rdata',sep=''), network)
    }
  }
}


load(paste('/Users/noni/Documents/elmn_package/DataLists/',sub.folders.short[2],'.Rdata',sep=''))






#create an igraph edgelist from the interactions.csv
graph_from_edgelist(el, directed = TRUE)


as.matrix(pivot_wider(network$interactions,names_from = 'attribute', values_from = 'value')[,c(2,4)])

graph_from_edgelist(as.matrix(pivot_wider(network$interactions,names_from = 'attribute', values_from = 'value')[,c(2,4)]))


readRDS('2_Environment_Benadi_JAE_2013')

use_data(network, overwrite = TRUE)

