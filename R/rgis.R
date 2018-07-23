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
#' \code{polygon_bounding_box} - Create a spatial polygon bounding box sf object from an extent and CRS
#' \code{buld_fishnet} - Build a vector polygon fishnet from reference object bounds and user-defined resolution
#'
#' \strong{Conversion Tools:}
#'
#' \code{polygon_to_raster} - Create a raster from a polygon sf object
#' \code{raster_to_polygon} - Create a polygon sf object from a raster
#' \code{ncdf_to_csv} - Convert NetCDF file to a CSV
#'
#' \strong{Input/Output (IO) Tools:}
#'
#' \code{import_shapefile} - Create an sf object from a shapefile
#' \code{import_raster} - Create a raster object from a file path
#' \code{import_ncdf_to_raster} - Import NetCDF to brick raster
#' \code{import_points_from_csv} - Import point data from a CSV file
#'
#'
#' \strong{Modification Tools:}
#'
#' \code{add_area_field} - Add and calculate an area field for polygon sf objects
#'
#' \strong{Workflow Tools:}
#'
#' \code{grid_to_zone_fraction} - Create fractional area of NetCDF grid cells to corresponding polygon features and vice versa
#'
#' @docType package
#' @name rgis
#' @importFrom dplyr %>%
NULL
