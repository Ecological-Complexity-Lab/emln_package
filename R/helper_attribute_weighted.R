#' Internal function - searches for a network which is weighted/unweighted in the database and returns the id of the networks that hit the search.
#'
#' @param ids ,a list to add the ids to
#' @keywords internal
#'

helper_attribute_weighted <- function(ids) {
  for (i in list.files("data")[-1]) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(if_else(all(grepl(filter(theloadedobjects$interactions, attribute == 'weight')$value, pattern = '1')),true = FALSE, false = TRUE)) { #check if the current network[i] is of the searched type
      ids <- c(ids, as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(ids)
}
