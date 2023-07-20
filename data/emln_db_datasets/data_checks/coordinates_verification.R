library(tidygeocoder)

coord_verification <- function(lats_longs,layers,names) {
  for (i in 1:length(names)) { #if no coordinates were found, then latitude,longitude == FALSE.
    if (length(is.na(lats_longs[[names[[i]]]]))>1) {
      layers <- rbind(layers, data.frame(layer = i, attribute = 'longitude', value = TRUE))
      layers <- rbind(layers, data.frame(layer = i, attribute = 'longitude', value = lats_longs$names)
    } else {
      layers <- rbind(layers, data.frame(layer = i, attribute = 'taxa_verified', value = FALSE))
    }
  }
  layers <- layers[order(layers$layer),] #order the layers by the layer
  return(layers)
  }
}




coord_verification_parse <- function(folders_list) {
  errors <- list()
  library(beepr)
  library(tidygeocoder)
  for (i in folders_list) {
    path_from <- i
    path_to <- gsub(' copy','',i)
    layers <- read_csv(paste(path_from,"/layers.csv",sep = ''))  %>% pivot_wider(id_cols = layer, names_from = attribute, values_from = value) %>% unnest()
    View(layers) #look at the layers to determine from which attribute to take the taxa from
    column <- readline(prompt="Enter column from which to check coordinates: ")
      lat_longs <- layers %>%
        geocode(column, method = 'osm', lat = latitude , long = longitude)
      layers <- lat_longs %>% pivot_longer(!layer, names_to = 'attribute', values_to = 'value', values_transform = list(value=as.character))
      coord_verification(lats_longs,layers,names)
      View(layers)
      beep()
      c = readline(prompt="Continue? (TRUE/FALSE): ") #if the layers are ok, write the csv
      if (c == TRUE) {
        write.csv(layers,paste(path_to,"/layers.csv", sep = ''), row.names = FALSE)
      } else if (exists('c')) {
        errors <- c(errors, i)
        next
    } else if (exists('b')) {
      errors <- c(errors, i)
      next
    }
      }
  return(errors)
}


##I cant feed the geocode function with a changing column name
