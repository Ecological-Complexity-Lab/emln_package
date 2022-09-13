#' View networks
#'
#'@name view_emln
#'
#' @description View the database interactively.
#'
#'
#' @return An interactive GUI for selecting networks.
#'
#' @export view_emln


view_emln <- function() {
  load('./data/descriptions.rda')
  DT::datatable(descriptions)
}

