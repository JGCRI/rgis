#' Conversion tools for rgis

library(fasterize)
library(raster)
library(spex)
library(sf)


#' Create a raster from a polygon sf object
#'
#' Creates a raster from a polygon sf object using the fasterize library and
#' function which provides great improvement over raster::rasterize. See
#' https://cran.r-project.org/web/packages/fasterize/fasterize.pdf
#'
#' @param sf an sf::sf() object with a geometry column of POLYGON and/or MULTIPOLYGON
#' objects
#' @param raster A raster object. Used as a template for the raster output. Can be created with
#' raster::raster()
#' @param field character. The name of a column in sf, providing a value for each of the polygons
#' rasterized. If NULL (default), all polygons will be given a value of 1
#' @param fun character. The name of a function by which to combine overlapping polygons.
#' Currently takes "sum", "first", "last", "min", "max", "count", or "any". Future
#' versions may include more functions or the ability to pass custom R/C++ functions.
#' If you need to summarize by a different function, useby= to get a RasterBrick
#' and then raster::stackApply() or raster::calc() to summarize
#' @param background numeric. Value to put in the cells that are not covered by any of the features of
#' x. Default is NA.
#' @param by character. The name of a column in sf by which to aggregate layers. If set,
#' fasterize will return a RasterBrick with as many layers as unique values of the by column
#' @return raster object
#' @export
polygon_to_raster <- function(raster, field = NULL, fun, background = NA_real_, by = NULL) {

  return(fasterize::fasterize(raster, field, fun, background, by))
}


#' Create a polygon sf object from a raster
#'
#' Create a polygon sf object from a raster, stack, or brick.
#'
#' @param raster A raster, stack, or brick object
#' @param na.rm boolean. If TRUE will polygonize only non-NA cells. Defualt is FALSE.
#'
#' @export
raster_to_polygon <- function(raster, na.rm = FALSE) {

  return(spex::qm_rasterToPolygons(raster, na.rm))
}
