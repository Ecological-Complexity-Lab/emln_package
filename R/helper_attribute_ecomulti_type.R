#' Internal function - searches for ecological/multilayer type entered in the database and returns the id of the networks that hit the search.
#'
#' @param attribute,ids ,the attribute searched for and a list to add the ids to
#' @keywords internal
#'
#'


helper_attribute_ecomulti_type <- function(attribute_temp,ids) {
  for (i in list.files("data")[-1]) {
    theloadedobjects <- loadRData(paste0('data/',i)) #load the networks by order
    if(tolower(attribute_temp) %in% tolower(theloadedobjects$description$value)) { #check if the current network[i] is of the searched type
      ids <- c(ids, as.numeric(gsub(i, pattern = '^emln|_.+$', replacement = '')))
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
    } else {
      rm(theloadedobjects) #delete the object that was loaded so there won't be 81 datasets loaded
      next
    }
  }
  return(ids)
}
