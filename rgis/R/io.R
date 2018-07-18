#' Input/Output IO tools for rgis

library(sf)
library(raster)


#' Create a raster object from a file path
#'
#' Create a raster object from a character full path to raster file with filename
#' and extension.
#'
#' @param raster_path character. A full path to the input raster file with filename and extnsion
#' @return raster object
#' @export
raster_object <- function(raster_path) {

  fasterize::raster(raster_path)
}
