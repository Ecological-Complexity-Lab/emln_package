## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)



#generate the Rdata for the DATA folder
library(ggplot2)
library(readr)
library(tibble)
library(dplyr)
library(readr)
library(stringr)


#read the file path
parent.folder <- "~/GitHub/emln_package/data-raw/"

sub.folders.short <- list.dirs(path = parent.folder, recursive = F, full.names = F)
sub.folders.long <- list.dirs(path = parent.folder, recursive = F, full.names = T)

#order the sub folders according to the id, using the short sub folders so I can use the id, but I eventually need the long to read the csvs
sub.folders.long <- sub.folders.long[str_order(sub.folders.short, numeric = T)]
sub.folders.short <- sub.folders.short[str_order(sub.folders.short, numeric = T)]


for (i in 1:length(sub.folders.long)) {
  if (file.exists(paste(sub.folders.long[[i]],'references.csv',sep = '/'))) {
    if (file.exists(paste(sub.folders.long[[i]],'state_nodes.csv',sep = '/'))) {
      network <- list(description = read_csv(paste(sub.folders.long[[i]],'description.csv',sep = '/')),
                      references = read_csv(paste(sub.folders.long[[i]],'references.csv',sep = '/')),
                      nodes = read_csv(paste(sub.folders.long[[i]],'nodes.csv',sep = '/')),
                      state_nodes = read_csv(paste(sub.folders.long[[i]],'state_nodes.csv',sep = '/')),
                      interactions = read_csv(paste(sub.folders.long[[i]],'interactions.csv',sep = '/')))
      class(network) <- 'EMLN'
      network_name <- sub.folders.short[i] #hanging network name
      assign(network_name, network) #assign it to the network variable
      do.call("use_data", list(as.name(network_name), overwrite = TRUE)) #save it with said name
    } else {
      network <- list(description = read_csv(paste(sub.folders.long[[i]],'description.csv',sep = '/')),
                      references = read_csv(paste(sub.folders.long[[i]],'references.csv',sep = '/')),
                      nodes = read_csv(paste(sub.folders.long[[i]],'nodes.csv',sep = '/')),
                      interactions = read_csv(paste(sub.folders.long[[i]],'interactions.csv',sep = '/')))
      class(network) <- 'EMLN'
      network_name <- sub.folders.short[i] #hanging network name
      assign(network_name, network) #assign it to the network variable
      do.call("use_data", list(as.name(network_name), overwrite = TRUE)) #save it with said name
    }
  } else {
    if (file.exists(paste(sub.folders.long[[i]],'state_nodes.csv',sep = '/'))) {
      network <- list(description = read_csv(paste(sub.folders.long[[i]],'description.csv',sep = '/')),
                      nodes = read_csv(paste(sub.folders.long[[i]],'nodes.csv',sep = '/')),
                      state_nodes = read_csv(paste(sub.folders.long[[i]],'state_nodes.csv',sep = '/')),
                      interactions = read_csv(paste(sub.folders.long[[i]],'interactions.csv',sep = '/')))
      class(network) <- 'EMLN'
      network_name <- sub.folders.short[i] #hanging network name
      assign(network_name, network) #assign it to the network variable
      do.call("use_data", list(as.name(network_name), overwrite = TRUE)) #save it with said name
    } else {
      network <- list(description = read_csv(paste(sub.folders.long[[i]],'description.csv',sep = '/')),
                      nodes = read_csv(paste(sub.folders.long[[i]],'nodes.csv',sep = '/')),
                      interactions = read_csv(paste(sub.folders.long[[i]],'interactions.csv',sep = '/')))
      class(network) <- 'EMLN'
      network_name <- sub.folders.short[i] #hanging network name
      assign(network_name, network) #assign it to the network variable
      do.call("use_data", list(as.name(network_name), overwrite = TRUE)) #save it with said name
    }
  }
}

##maybeuselater##saveRDS(file=paste('~/GitHub/emln_package/data/',sub.folders.short[i],'.Rdata',sep=''), network)
load(paste('/Users/noni/Documents/elmn_package/DataLists/',sub.folders.short[2],'.Rdata',sep=''))



for(i in 1:81) {                    # Head of for-loop
  network_name <- sub.folders.short[i]
  assign(network_name, network)
  do.call("use_data", list(as.name(network_name), overwrite = TRUE))
}

#create an igraph edgelist from the interactions.csv
graph_from_edgelist(el, directed = TRUE)


as.matrix(pivot_wider(network$interactions,names_from = 'attribute', values_from = 'value')[,c(2,4)])

graph_from_edgelist(as.matrix(pivot_wider(network$interactions,names_from = 'attribute', values_from = 'value')[,c(2,4)]))


readRDS('2_Environment_Benadi_JAE_2013')

usethis::use_data(network, overwrite = TRUE)



library(usethis)
network_name <- sub.folders.short[i] #hanging network name
assign(network_name, network) #assign it to the network variable
do.call("use_data", list(as.name(network_name), overwrite = TRUE)) #save it with said name
