
#' Ecological Network Type (eco)
#'
#' Get the ecological network type of a network
#' @param emln object (emln multilayer network)
#' @return String containing the ecological network type
#' @examples
#' \dontrun{
#' Environment_Schleuning_2010 <- readRDS('Environment_Schleuning_2010')
#' emln1 <- eco(Environment_Schleuning_2010)
#' print(emln1)
#' 'Pollination'
#' }
#' @export eco

eco <- function(emln) {
  UseMethod("EcoType")
}

eco.EMLN <- function(emln) {
  cat(emln$description[emln$description$attribute == 'ecological_network_type',]$value)
}

