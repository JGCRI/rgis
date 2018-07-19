# Tools to modify raster or sf objects

library(sf)
library(lwgeom)

#' Add and calculate an area field for polygon sf objects
#'
#' Add and calcualte an area field for polygon sf objects.  Area is calcuated
#' based on the linear unit of the assigned projection of the input sf object.
#'
#' @param sf_obj An polygon sf object
#' @param field_name character. The name of the area field.
#' @return Modified sf object
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
add_area_field <- function(sf_obj, field_name) {

  sf_obj[field_name] <- sf::st_area(sf_obj$geometry)

  return(sf_obj)
}
