#' rgis:  R-based Geographic Information System (GIS) utilities
#'
#' The \code{rgis} package facilitates GIS functionality and workflows
#' commonly reprented in propritary and open source GISs. This package
#' does not contain functionality for visulization, but is focsed on
#' geospatial algorithms for analysis, conversion, IO, modification, and
#' workflows of spatial files and data structures.
#'
#' @section Currently Supported
#'
#' \strong{Analysis Tools:}
#'
#' polygon_bounding_box - Create a spatial polygon bounding box sf object from an extent and CRS
#' buld_fishnet - Build a vector polygon fishnet from reference object bounds and user-defined resolution
#'
#' \strong{Conversion Tools:}
#'
#' polygon_to_raster - Create a raster from a polygon sf object
#' raster_to_polygon - Create a polygon sf object from a raster
#' ncdf_to_csv - Convert NetCDF file to a CSV
#'
#' \strong{Input/Output (IO) Tools:}
#'
#' import_shapefile - Create an sf object from a shapefile
#' import_raster - Create a raster object from a file path
#' import_ncdf_to_raster - Import NetCDF to brick raster
#' import_points_from_csv - Import point data from a CSV file
#'
#'
#' \strong{Modification Tools:}
#'
#' add_area_field - Add and calculate an area field for polygon sf objects
#'
#' \strong{Workflow Tools:}
#'
#' grid_to_zone_fraction - Create fractional area of NetCDF grid cells to corresponding polygon features and vice versa
#'
#' @docType package
#' @name rgis
NULL
