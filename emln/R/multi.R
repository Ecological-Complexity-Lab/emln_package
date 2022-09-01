
#' Multilayer Network Type (multi)
#'
#' Get the multilayer network type of a network
#' @param emln object (emln multilayer network)
#' @return String containing the multilayer network type
#' @examples
#' \dontrun{
#' Environment_Schleuning_2010 <- readRDS('Environment_Schleuning_2010')
#' emln1 <- eco(Environment_Schleuning_2010)
#' print(emln1)
#' 'Environment'
#' }
#' @export multi

multi <- function(object) {
  UseMethod("multi")
}

multi.EMLN <- function(object) {
  cat(object$description[object$description$attribute == 'multilayer_network_type',]$value)
}


