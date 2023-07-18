#' View networks
#'
#' @name view_emln
#'
#' @description View the database interactively.
#'
#' @return An interactive GUI for browinsg through networks.
#'
#' @export view_emln
#' @importFrom DT datatable



view_emln <- function() {
  #load('./data/descriptions.rda')
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop(
      "Package \"DT\" must be installed to use this function.",
      call. = FALSE
    )
  }

  datatable(descriptions,rownames = F)
}


