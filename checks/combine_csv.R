library(tidyverse)
library(bipartite)




# Manually enter the directories for the sub tasks
my_dirs <- c("/Users/ofir/GitHub/emln_db/EMLN_Datasets/ofir", 
             "/Users/ofir/GitHub/emln_db/EMLN_Datasets/noa")
             
# Grab all files from the directories using list.files in sapply
files <- sapply(my_dirs, list.files, full.names = TRUE)

# Your output directory to copy files to
new_dir <- "/Users/ofir/GitHub/emln_db/EMLN_Datasets/all_files"
# Make sure the directory exists
dir.create(new_dir, recursive = TRUE)

# Copy the files
i<-1
for(directories in files) {
  for(dir in directories){
    csv_layers<-paste0(dir,"/layers.csv")
    csv_interactions<-paste0(dir,"/interactions.csv")
    csv_nodes<-paste0(dir,"/nodes.csv")
    csv_references<-paste0(dir,"/references.csv")
    
    # See ?file.copy for more options
    to<-paste0(new_dir,"/layers_",i,".csv")
    file.copy(csv_layers, to)
    to<-paste0(new_dir,"/interactions_",i,".csv")
    file.copy(csv_interactions, to)
    to<-paste0(new_dir,"/nodes_",i,".csv")
    file.copy(csv_nodes, to)
    to<-paste0(new_dir,"/references_",i,".csv")
    file.copy(csv_references, to)
    i<-i+1
  }
  
}

