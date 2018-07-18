# Conversion tools for rgis

library(fasterize)
library(raster)
library(spex)
library(sf)
library(ncdf4)


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


#' Convert NetCDF file to a CSV
#'
#' Convert NetCDF file to a CSV
#'
#' @param ncdf_file character. asdf
#' @param out_csv character. asdf
#' @param nc_var_name character. asdf
#' @param resolution float. asdf
#' @param start_year int
#' @param through_year int
#' @param nmonths int
#' @author Caleb Braun (caleb.braun@pnnl.gov)
#' @export
ncdf_to_csv <- function(ncdf_file, out_csv, nc_var_name, resolution = 0.5, start_year = NULL, through_year = NULL, nmonths = NULL) {

  n <- nc_open(ncdf_file)
  cell_vals <- ncvar_get(n, nc_var_name)
  nc_close(n)

  lon_ext = dim(cell_vals)[1]
  lat_ext = dim(cell_vals)[2]

  # use all months in file if no setting
  if (is.null(nmonths) & is.null(start_year) & is.null(through_year)) {
    nmonths <- dim(cell_vals)[3] # Assuming lat/lon/month
  }

  # if using start and through years, calc months
  else if (!is.null(start_year) & !is.null(through_year) & is.null(nmonths)) {
    nmonths <- (through_year - start_year) + 1 * 12
  }

  # build output matrix
  template <- expand.grid(seq(0, lat_ext-resolution, by = resolution),
                          seq(0, lon_ext-resolution, by = resolution),
                          1:nmonths)
  combined <- cbind(template, as.vector(cell_vals[ , , 1:nmonths]))
  names(combined) <- c('lat', 'lon', 'month', nc_var_name)

  return(combined)
}

