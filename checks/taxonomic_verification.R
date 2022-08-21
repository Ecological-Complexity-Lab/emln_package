taxa_verification <- function(classification_var,nodes,names) {
  for (i in 1:length(names)) { #if no id was found, then taxa_verified == FALSE. If any id is found in the species rank (because the values given were species names), taxa_verified == TRUE.
    if (length(is.na(classification_var[[names[[i]]]]))>1) {
      if (grepl(names[[i]], 'unidentified|Unidentified', fixed = TRUE)) { #there are cases where taxize recognizes 'unidentified' as a taxa. which is not correct.
        nodes <- rbind(nodes, data.frame(node_id = i, attribute = 'taxa_verified', value = FALSE))
        next
      }
      nodes <- rbind(nodes, data.frame(node_id = i, attribute = 'taxa_verified', value = TRUE))
      nodes <- rbind(nodes, data.frame(node_id = i, attribute = 'verification_level', value = classification_var[[names[[i]]]][['rank']][length(classification_var[[names[[i]]]][['rank']])]))
    } else {
      nodes <- rbind(nodes, data.frame(node_id = i, attribute = 'taxa_verified', value = FALSE))
    }
  }
  nodes <- nodes[order(nodes$node_id),] #order the nodes by the node_id
  return(nodes)
}




taxa_verification_parse <- function(folders_list) {
  errors <- list()
  for (i in folders_list) {
    path_from <- i
    path_to <- gsub(' copy','',i)
    nodes <- read_csv(paste(path_from,"/nodes.csv",sep = ''))
    View(nodes) #look at the nodes to determine from which attribute to take the taxa from
    beep()
    a = readline(prompt="Enter attribute from which to check classification: ") #give the attribtue name to check
    names <- gsub('([[:blank:]]|[[:punct:]])((sp|spp|sp[1-9+])[^a-zA-Z].*$|(sp|spp|sp[1-9+])$)','',nodes[nodes$attribute == a, ]$value) #remove 'sp' string if it appears, it throws off the classification function.
    names <- gsub('[[:punct:]]',' ',names) #create a list of taxa names to verify
    View(names) #check that the names are ok
    beep()
    b = readline(prompt="Continue? (TRUE/FALSE): ") #if they are ok, continue, else, skip and add to errors list
    if (b == TRUE) {
      classif <- classification(names, db = "ncbi",max_tries = 1, max_batch = 1, row = 1) #check the scientific name in the ncbi database and find its associated id. 
      nodes <- taxa_verification(classif,nodes,names)
      View(nodes)
      beep()
      c = readline(prompt="Continue? (TRUE/FALSE): ") #if the nodes are ok, write the csv
      if (c == TRUE) {
        write.csv(nodes,paste(path_to,"/nodes.csv", sep = ''), row.names = FALSE)
      } else if (exists('c')) {
        errors <- c(errors, i)
        next
      }
    } else if (exists('b')) {
      errors <- c(errors, i)
      next
    }
  }
  return(errors)
}

