# Remove all objects, including functions
rm(list=ls())
# Remove all objects, except functions
rm(list = setdiff(ls(), lsf.str()))

# library packages
prep.packages = function(package.list) {
  loaded = package.list %in% .packages()
  if ( all(loaded) ) return(invisible())
  
  package.list = package.list[!loaded]
  installed = package.list %in% .packages(TRUE)
  if ( !all(installed) ) install.packages(package.list[!installed])
  for ( p in package.list )
  {
    print(paste("Loading package:",p))
    library(p,character.only=TRUE)
  }
}

rmatrix <- function(r=10,c=6,prob=0.5){
  return(matrix(rbinom(r*c,1,0.5),r,c))
}

namelist <- function(df){
  data.frame(nameID=1:length(df),name=names(df))
}

ggplotToBrowser <- function(p, w=35, h=16.875) {
  ggsave(filename = tf_img <- tempfile(fileext = ".svg"), plot = p, width=w, height=h, units = 'cm')
  html <- sprintf('<html><body><img src="%s"></body></html>', paste0("file:///", tf_img))
  cat(html, file = tf_html <- tempfile(fileext = ".html"))
  if(Sys.info()[1]=="Linux"){
    options(browser="google-chrome")
    browseURL(tf_html)  
  } else {
    system(sprintf("open %s", tf_html))  
  }
}

matrix2list <- function(m,layerID=0,nodelist){
  #  Iterate over half a matrix, do not include diagonal
  node_i <- node_j <- w <-  NULL
  q <- NROW(m)
  for (col in 1:(q-1)){
    for (row in (col+1):q){
      i <- rownames(m)[row]
      j <- rownames(m)[col]
      if(m[i,j]!=0){
        node_i <- c(node_i, i)
        node_j <- c(node_j, j)
        w <- c(w, m[i,j])
      }
    }
  }
  edgelist <- data.frame(layer=rep(layerID,length(w)),
                         i=node_i,
                         j=node_j,
                         w=w, stringsAsFactors = F)
  edgelist$i <- nodelist$nodeID[match(edgelist$i,nodelist$StudyID)]
  edgelist$j <- nodelist$nodeID[match(edgelist$j,nodelist$StudyID)]
  return(edgelist)
}

df2matrix <- function(df){
  rownames(df) <- df[,1]
  df <- df[,-1]
  df <- data.matrix(df)
  return(df)
}

lowmat <- function(m){
  return(m[lower.tri(m)])
}

head2 <- function(x, n=5){
  if (class(x) == "matrix"){print(x[1:n,1:n])}
  else
    if (class(x) == "data.frame"){print(head(x,n))}
  else {print('x is not a matrix or data frame')}
}

gdensity <- function(m, bipartite=F){
  if (bipartite==F){
    no_possible_edges=NROW(m)*(NROW(m)-1)/2
    return(length(which(m!=0))/no_possible_edges)
  } else {return(length(which(m!=0))/(NROW(m)*NCOL(m)))}
}

# program this function for bipartite networks!
layerInfo.bipartite <- function(layers){
  li <- data.frame(Layer=c('var','ms','geo'),
                   Nodes=sapply(layers, NROW),
                   Density=round(sapply(layers, gdensity),3))
  return(li)
}

binarize <- function(m){
  m[m>0] <- 1
  return(m)
}

# This function creates files for a general multilayer format to use in MuxViz. 
# The input is a list of matrices, each representing a layer. The matrices 
# should have the exact same row and column names, which include all the nodes 
# in the system. If a node does not appear in the layer there should just be a 0
# entry in the matrix
createGeneralMultilayer <- function(nets,layernames,outfolder=getwd(),outname='multilayer',coords=NULL,nodeColors=NULL){
  nodes.list <- sapply(nets, rownames)
  # Make sure that all the rownames are the same across the matrices
  similarity.matrix<-apply(nodes.list,2,function(x)colSums(x==nodes.list))
  if (any(similarity.matrix!=NROW(nodes.list))){stop('All matrices must have the same rows in the same order')}
  # Write the config file
  outfile=paste(outfolder,outname,'_config.txt',sep='')
  cat(paste(outfolder,outname,'.edges;',sep=''), file=outfile, append=F)
  cat(paste(outfolder,outname,'_layers.txt;',sep=''), file=outfile, append=T)
  cat(paste(outfolder,outname,'_layout.txt',sep=''), file=outfile, append=T)
  # Write the layers file
  outfile=paste(outfolder,'/',outname,'_layers.txt',sep='')
  layers.df <- data.frame(layerID=1:length(nets),layerLabel=layernames)
  write.table(layers.df,outfile,row.names = F, quote = F)
  # Write the layout (nodes) file
  outfile=paste(outfolder,'/',outname,'_layout.txt',sep='')
  if (!is.null(coords)){
    names(coords) <- c('node','Long','Lat')
    nodes.layout <- data.frame(nodeID=1:NROW(nodes.list),
                               nodeLabel=nodes.list[,1],
                               nodeLong=coords$Long[match(nodes.list[,1],coords$node)],
                               nodeLat=coords$Lat[match(nodes.list[,1],coords$node)])
  } else {
    nodes.layout <- data.frame(nodeID=1:NROW(nodes.list),
                               nodeLabel=nodes.list[,1])
  }
  write.table(nodes.layout,outfile,row.names = F, quote = F)
  
  # Write the edges file. This includes both inter- and intra-edges. The format is:
  # node layer node layer strength
  edges <- NULL # Initialize the edge list
  
  #Build the INTRALAYER edges for each layer
  for (l in 1:length(nets)){
    print(paste('Building edge list for layer',l))
    current.layer <- nets[[l]]
    # Using the commented code below it is possible to iterate over half a matrix if the network is 
    # undirected, for efficiency. Otherwise just go through the whole matrix. Inefficient but more straightforward.
    #     if (isSymmetric(current.layer) | all(current.layer[upper.tri(current.layer)]==0) | all(current.layer[lower.tri(current.layer)]==0)){
    #       print(paste('Layer ',l,' is undirected'))
    #       q <- NROW(current.layer)
    #       for (cl in 1:(q-1)){
    #         for (rw in (cl+1):q){
    #           if (current.layer[rw,cl]!=0){edges <- rbind(edges, c(cl,l,rw,l,current.layer[rw,cl]))}
    #         }
    #       }
    #     } else {
    #       print(paste('Layer ',l,' is directed'))
    #       for (rw in 1:NROW(current.layer)){
    #         for (cl in 1:NCOL(current.layer)){
    #           if (current.layer[rw,cl]!=0){edges <- rbind(edges, c(cl,l,rw,l,current.layer[rw,cl]))}
    #         }
    #       }
    #     }
    for (rw in 1:NROW(current.layer)){
      for (cl in 1:NCOL(current.layer)){
        if (current.layer[rw,cl]!=0){edges <- rbind(edges, c(cl,l,rw,l,current.layer[rw,cl]))}
      }
    }
  } # End loop for writing intralayer edges
  
  #Build the INTERLAYER edges for each layer
  for (n in nodes.layout$nodeID){
    #The layers in which a node appears
    inlayer <- unique(as.vector(edges[edges[,1]==n | edges[,3]==n,c(2,4)]))
    if (length(inlayer)>1) # When a node appears in more than one layer
    {(edge.combinations <- combn(inlayer, m = 2, simplify = T)) # Combinations of all layers where a node appears
      layer.pair.edges <- cbind(rep(n, dim(edge.combinations)[2]),
                                edge.combinations[1,],
                                rep(n, dim(edge.combinations)[2]),
                                edge.combinations[2,],
                                rep(1, dim(edge.combinations)[2])) #interlayer edge weight is always 1
      edges <- rbind(edges, layer.pair.edges) } else {print(paste('node ',n,'(',nodes.list[n,1],')','is only in layer',inlayer))}
  }
  # Write the edges file
  outfile=paste(outfolder,'/',outname,'.edges',sep='')
  write.table(edges,outfile,row.names = F, col.names=F, quote = F)
  
  # Write the external colors file
  if(!is.null(nodeColors)){
    print('Writing the external node colors file...')
    outfile=paste(outfolder,'/',outname,'_node_colors.txt',sep='')
    write.table(nodeColors,outfile,sep=' ',row.names = F,col.names = T, quote = F)
  }
  
  # Return the multilayer object
  return(list(layers=layers.df,nodes=nodes.layout,edge_list=edges))
} # End function

prep.packages(c("igraph","bipartite","ecodist","ggplot2","stringr","ecodist", "Imap","RgoogleMaps","RColorBrewer","googlesheets"))

# Read the 




# Make connection to the google sheets docs
(my_sheets <- gs_ls())
# (expect a prompt to authenticate with Google interactively HERE)
# require(dplyr);my_sheets %>% glimpse()
my_sheets$sheet_title
## Load data frames
flea.df <- gs_title('Q_FleaDataFrame') 
flea.df <- gs_read(flea.df)
flea.df$Date <- as.Date(flea.df$Date_, "%d/%m/%Y")
flea.df <- flea.df[,-c(1,2,3,4,6,7)]
namelist(flea.df)
# Work at the species level: sum the number of fleas recovered from all
# individuals of a particular host in a particualr place and time
flea.df.splevel <- aggregate(.~Locality+Date+Host,data=flea.df,sum)
flea.df.splevel <- flea.df.splevel[with(flea.df.splevel, order(Locality, Host, Date)),]
# Also record the number of indivisuals of a particular host captured in a
# particualr place and time. This is to calcualte parasite average abundance on
# a given host
flea.host.abund <- aggregate(.~Locality+Date+Host,data=flea.df[,c(1,2,3,22)], length)
names(flea.host.abund)[4] <- 'Abundance'
# flea.df.splevel$CaseID <- 1:length(flea.df.splevel[,1])
namelist(flea.df.splevel)
# flea.df$Day <- format(flea.df$Date, '%d')
# flea.df$Month <- format(flea.df$Date, '%m')
# flea.df$Year <- format(flea.df$Date, '%Y')
#--------
mite.df <- gs_title('Q_MiteDataFrame') 
mite.df <- gs_read(mite.df)
mite.df$Date <- as.Date(mite.df$Date_, "%d/%m/%Y")
mite.df <- mite.df[,-c(1,2,3,4,6,7)]
namelist(mite.df)
# Work at the species level: sum the number of fleas recovered from all
# individuals of a particular host in a particualr place and time
mite.df.splevel <- aggregate(.~Locality+Date+Host,data=mite.df,sum)
mite.df.splevel <- mite.df.splevel[with(mite.df.splevel, order(Locality, Host, Date)),]
# Also record the number of indivisuals of a particular host captured in a
# particualr place and time. This is to calcualte parasite average abundance on
# a given host
mite.host.abund <- aggregate(.~Locality+Date+Host,data=mite.df[,c(1,2,3,99)], length)
names(mite.host.abund)[4] <- 'Abundance'
# mite.df.splevel$CaseID <- 1:length(mite.df.splevel[,1])+max(flea.df.splevel$CaseID)
namelist(mite.host.abund)

# Combine the data frames of fleas nad mites.
# THIS ASSSUMES THAT EACH CAPTURED SPECIMEN WAS EXAMINED BOTH FOR FLEAS AND MITES
fulldata <- merge(flea.df.splevel, mite.df.splevel, by=c('Locality','Date','Host'), all=T)
fulldata[is.na(fulldata)] <- 0 # Assume a NA means no parasites were found
fulldata.host.abund=merge(mite.host.abund,flea.host.abund,by=c('Locality','Date','Host'), all=T)
fulldata.host.abund$Abundance.x[is.na(fulldata.host.abund$Abundance.x)] = fulldata.host.abund$Abundance.y[is.na(fulldata.host.abund$Abundance.x)]
fulldata.host.abund$Abundance.y[is.na(fulldata.host.abund$Abundance.y)] = fulldata.host.abund$Abundance.x[is.na(fulldata.host.abund$Abundance.y)]
fulldata <- merge(fulldata,fulldata.host.abund[,1:4],by=c('Locality','Date','Host'))
names(fulldata)[length(fulldata)] <- 'HostAbundance'
namelist(fulldata)

## Load locality data
populations <- gs_title('Populations') # The sheet to where results will be written
populations <- gs_read(populations)
names(populations)
localities <- aggregate(Locality ~ Region+Latitude+Longitude, data = populations,unique)
# Remove the two localities Hermanovce, Kokosovce because they have the same coordinates but geographically they are far apart...
localities <- localities[-which(sapply(localities$Locality, length)>1),]
localities$Locality <- unlist(localities$Locality)
fulldata <- fulldata[fulldata$Locality != 'Hermanovce',]
fulldata <- fulldata[fulldata$Locality != 'Kokosovce',]
fulldata.host.abund <- fulldata.host.abund[fulldata.host.abund$Locality != 'Hermanovce',]
fulldata.host.abund <- fulldata.host.abund[fulldata.host.abund$Locality != 'Kokosovce',]

fulldata <- merge(fulldata, localities, by='Locality', all.x=T)
namelist(fulldata)
fulldata <- fulldata[,c(120,1,121,122,2,3,119,4:118)]

## Load species data
parasites <- gs_title('ListParasites') 
parasites <- gs_read(parasites)
names(parasites)
hosts <- gs_title('ListMammals') 
hosts <- gs_read(hosts)
names(hosts)
hosts$nodeID <- 1:length(hosts$HostShortName)
parasites$nodeID <- 1:(length(parasites$ParasShortName))+max(hosts$nodeID)
# Change species names to Scientific names
names(fulldata)[8:length(fulldata)] <- parasites$ParasSciName[match(names(fulldata)[8:length(fulldata)],parasites$ParasShortName)]

###### The multilayer stuff!! Split data by space, time...
fulldata.space <- fulldata[with(fulldata, order(Region,Locality,Host)),]
fulldata.space <- aggregate(.~Region+Locality+Latitude+Longitude+Host,data=fulldata[,-5],sum) # without the date column
fulldata.space <- fulldata.space[with(fulldata.space, order(Region,Locality,Host)),]

# Split data by region. Each multilayer network is defined per region. That is, each individual layer is a locality within the region
# Each multilayer is stored as list of matrices.

MLNs.region <- split(fulldata.space, fulldata.space$Region)
names(MLNs.region)

MLNs.region.to.consider <- which(sapply(MLNs.region, function(x) length(unique((x$Locality))))>2)
for (r in MLNs.region.to.consider){
  MLN <- MLNs.region[[r]]
  MLN <- aggregate(.~Locality+Host,data = MLN[,c(2,5:length(MLN))], sum)
  MLN <- MLN[with(MLN, order(Locality,-HostAbundance)),]
  # Remove all the rows that sum to zero
  MLN=MLN[-which(rowSums(MLN[,4:length(MLN)])==0),]
  # Remove all the columns that sum to zero
  cols <- !names(MLN) %in% names(which(colSums(MLN[,4:length(MLN)])==0))
  MLN=MLN[,cols]
  # Calculate mean parasite abundance as the total number of parasites of a given
  # species recovered from a given host speceis at a given locality
  MLN <- cbind(MLN[,1:2], sweep(MLN[,4:length(MLN)], MARGIN = 1, STATS = MLN[,3], FUN = '/'))
  # Create the MLN as a 3-D array.
  rw <- unique(MLN$Host)
  cl <- unique(names(MLN)[3:length(MLN)])
  layers <- unique(MLN$Locality)
  MLN <- split(MLN, MLN$Locality)
  MLN <- sapply(MLN, function(x) df2matrix(x[,2:length(x)]))
  MLN.array <- array(0, dim = list(length(rw),length(cl),length(layers)), dimnames = list(rw,cl,layers))
  dim(MLN.array)
  for (l in 1:length(MLN)){
    MLN.array[rownames(MLN[[l]]),colnames(MLN[[l]]),l] <- MLN[[l]]
  }
  assign(paste('MLN_',r,sep=''),MLN.array)
}

any(apply(MLN_8, 1, sum)==0)
any(apply(MLN_8, 2, sum)==0)
any(apply(MLN_8, 3, sum)==0)





