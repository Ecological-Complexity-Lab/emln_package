library(stringr)
parent.folder <- "~/GitHub/emln_db/EMLN_Datasets/datasets"

setwd("~/GitHub/emln_db/EMLN_Datasets/datasets") ##in this chunk, we want the working directory to be the one with all the networks in order to read them

sub.folders.short <- list.dirs(path = parent.folder, recursive = T, full.names = F)[-1]
sub.folders.long <- list.dirs(path = parent.folder, recursive = T, full.names = T)[-1]

#order the sub folders according to the id, using the short sub folders so I can use the id, but I eventually need the long to read the csvs
sub.folders.long <- sub.folders.long[str_order(sub.folders.short, numeric = T)]


sub.folders.long_copy <- sub.folders.long

interaction_type <- list() #list of all the interaction types in all the networks
for(i in 1:length(sub.folders.long)) {
  interaction<-read.csv(paste0(sub.folders.long[i],"/interactions.csv"))
  interaction_type[[i]] <- unique(interaction[interaction$attribute == 'type',]$value)
  rm(interaction)
}

#types_of_interactions <- unique(unlist(interaction_type)) #get all the interaction types that exist in all the networks in order to allocate them to a specific network type

# types_of_interactions_to_network_type <- c('Pollination' = 'pollination',
#                                            'Seed-Dispersal' = 'frugivory',
#                                            'Plant-Ant' = 'plant-ant',
#                                            'Plant-Herbivore' = 'herbivory',
#                                            'Host-Parasite' = 'host-parasite',
#                                            'Food-Web' = 'predation',
#                                            'Anemone-Fish' = 'anemone-fish',
#                                            'Multiples' = 'competition')
# Food_web <- c('predation', 'trophic','scavenger', 'detritivore', 'negative', 'positive')
#create a list of 81 elements that corresponds to the 81 networks and for each network, the list contains the ecological network type
assign_network_types <- function(interaction_types_per_network) {
  ecological_networks_types <- list()
  for (i in 1:81) {
    if (all(grepl('pollination', interaction_types_per_network[[i]]))) {
      ecological_networks_types[[i]] <- 'Pollination'
      next
    } 
    if (all(grepl('frugivory', interaction_types_per_network[[i]])) | all(grepl('seed-dispersal',interaction_types_per_network[[i]])) | all(c('seed-dispersal','frugivory') %in% interaction_types_per_network[[i]])) {
      ecological_networks_types[[i]] <- 'Seed-Dispersal'
      next
    } 
    if (all(grepl('plant-ant', interaction_types_per_network[[i]]))) {
      ecological_networks_types[[i]] <- 'Plant-Ant'
      next
    }
    if (all(grepl('host-parasite', interaction_types_per_network[[i]])) | all(grepl('parasitism', interaction_types_per_network[[i]])) | all(c('host-parasite','parasitism') %in% interaction_types_per_network[[i]])) {
      ecological_networks_types[[i]] <- 'Host-Parasite'
      next
    } 
    if (all(grepl('plant-herbivore', interaction_types_per_network[[i]])) | all(grepl('herbivory', interaction_types_per_network[[i]]))){
      ecological_networks_types[[i]] <- 'Plant-Herbivore'
      next
    }
    if (all(grepl('anemone-fish', interaction_types_per_network[[i]]))) {
      ecological_networks_types[[i]] <- 'Anemone-Fish'
      next
    } 
    layer <- read.csv(paste0(sub.folders.long[i],"/layers.csv"))
    if ('directed' %in% layer$attribute) {
      print(i)
      if (unique(layer[layer$attribute == 'directed',]$value) == 'TRUE') { #the true is a string not boolean
        print(i)
        ecological_networks_types[[i]] <- 'Food-Web'
      next
     } else {
          next
        }
    }
    if (all(c('predation','herbivory') %in% interaction_types_per_network[[i]] &  !any('pollination' %in% interaction_types_per_network[[i]])) | all(c('host-parasite','trophic') %in% interaction_types_per_network[[i]]) & !any('pollination' %in% interaction_types_per_network[[i]])) {
      ecological_networks_types[[i]] <- 'Food-Web'
      next
    }
    ecological_networks_types[[i]] <- 'Multiples'
}
  return(ecological_networks_types)
}


ecological_networks_types_test <- assign_network_types(interaction_type)


#load the description csv of each network and add the ecological network type, also change the type to network type
update_description <- function(number_of_networks,path) {
  for (i in 1:number_of_networks) {
    setwd(path)
    description_temp <- read.csv(paste(sub.folders.long_copy[i],'/description.csv',sep = ''))
    description_temp[description_temp$attribute == 'type',]$attribute <-'multilayer_network_type'
    description_temp <- rbind(description_temp, data.frame(attribute = 'ecological_network_type', value = ecological_networks_types[[i]]))
    write.csv(file = paste(sub.folders.long[i],'/description.csv',sep = ''), x = description_temp,row.names=FALSE)
    }
  }
update_description(81,"~/GitHub/emln_db/EMLN_Datasets/datasets")

for (i in 1:81) {
    description_temp <- read.csv(paste(sub.folders.long[i],'/description.csv',sep = ''))
    description_temp[description_temp$attribute == 'type',]$attribute <-'multilayer_network_type'
    description_temp <- rbind(description_temp, data.frame(attribute = 'ecological_network_type', value = ecological_networks_types[[i]]))
}


update_ecological_network_type <- function() {
  for (i in 1:81) {
  description_temp <- read.csv(paste(sub.folders.long[[i]],'/description.csv',sep = ''))
  description_temp[description_temp$attribute == 'ecological_network_type',]$value <- ecological_networks_types_test[[i]]
  write.csv(file = paste(sub.folders.long[[i]],'/description.csv',sep = ''), x = description_temp,row.names=FALSE)
  }
}


update_ecological_network_type()



#validate the specification
check <- list()


for (i in 1:81) {
  name <- ecological_networks_types_test[[i]]
  value = interaction_type[[i]]
  check <- c(check,list(value))
}


names(check) <- ecological_networks_types_test
