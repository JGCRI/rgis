# Input/Output IO tools for rgis

library(sf)
library(raster)
library(fasterize)

#' Create an sf object from a shapefile
#'
#' Create an sf object from a full path to shapefile with file name and extension
#'
#' @param shp_path character. A full path to the input shapefile with file name and extension
#' @return sf object
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
import_shapefile <- function(shp_path, quiet = TRUE) {

  return(sf::st_read(shp_path, quiet = quiet))
}


#' Create a raster object from a file path
#'
#' Create a raster object from a full path to raster file name and extension
#'
#' @param raster_path character. A full path to the input raster file with file name and extension
#' @return raster object
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
import_raster <- function(raster_path) {

  return(fasterize::raster(raster_path))
}


#' Import NetCDF to brick raster
#'
#' Import NetCDF to brick raster
#'
#' @param ncdf_path character. A full path to the input NetCDF file with the file name and extension
#' @return raster object
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
import_ncdf_to_raster <- function(ncdf_file) {

  return(raster::brick(ncdf_file))
}

#' Import point data from a CSV file
#'
#' Import point data that contains a value to be spatially joined to the fishnet containing
#' fractional area.  May either be a shapefile or a CSV file containing a latitude and longitude
#' for each record.
#'
#' @param f character. The full path with filename and extension to the points dataset.
#' @param pts_lat_field character. The field name for latitude
#' @param pts_lon_field character. The field name for longitude
#' @param pts_crs int. The native EPSG number for the coordinate reference system used in the
#' creation of the input points data. The default is 4326 (WGS 1984).
#' @param my_crs int. The EPSG number of the desired coordinate reference system. The
#' default is EPSG:3857 the WGS 84 / Pseudo-Mercator -- Used by all modern web
#' mapping applications.
#' @return A simple features (sf) spatial data frame object.
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
import_points_from_csv <- function(f, pts_lat_field, pts_lon_field, pts_crs = 4326, my_crs = 3857) {

  pts <- read.csv(file = f, header = TRUE, sep = ',')

  # change latitude, longitude columns to numeric
  cols.num <- c(pts_lat_field, pts_lon_field)
  pts[cols.num] <- sapply(pts[cols.num], as.numeric)

  # convert to sf spatial data frame object and transform to target CRS
  pts.SP <- sf::st_as_sf(pts, coords = c(pts_lat_field, pts_lon_field), crs = pts_crs) %>%
    sf::st_transform(crs = my_crs)

  return(pts)
}
