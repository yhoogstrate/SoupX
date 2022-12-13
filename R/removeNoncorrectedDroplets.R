#' Removes droplets from the SoupChannel object not actually corrected
#'
#' @note
#' This is a cleanup function
#'
#' @export
#' @param sc A SoupChannel object
#' @return A SoupChannel object.
removeNoncorrectedDroplets = function(sc){
  sc$tod=NULL
  sc$soupDroplets  = NULL
  return(sc)
}

