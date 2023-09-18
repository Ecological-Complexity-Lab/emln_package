tryCatch(
  expr = {
    ref <- read.csv('references.csv')
    if (NA %in% ref$doi) {
      no_doi <- append(no_doi,i)
    } 
  },
  error = function(e){ 
    message(paste('there is no references.csv in:', i))
  }
)
