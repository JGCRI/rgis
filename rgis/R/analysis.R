#' Analysis tools for rgis

library(dplyr)
library(sf)
library(tools)
library(lwgeom)


#' Create a spatial polygon bounding box sf object
#'
#' Creates a spatial polygon bounding box from a user-provided extent
#' and coordinate reference system.
#'
#' @param x_min A float or integer value for the x (longitude) coordinate minimum
#' @param x_max A float or integer value for the x (longitude) coordinate maximum
#' @param y_min A float or integer value for the y (latitude) coordinate minimum
#' @param y_max A float or integer value for the y (latitude) coordinate maximum
#' @param my_crs An integer for the EPSG number of the desired output coordinate
#' reference system.
#' @return A bounding box polygon as an sf object
#' @export
polygon_bounding_box <- function(x_min, x_max, y_min, y_max, my_crs) {

  bbox <- matrix(c(x_min, y_max,
                   x_max, y_max,
                   x_max, y_min,
                   x_min, y_min,
                   x_min, y_max), byrow = TRUE, ncol = 2) %>%
          list() %>%
          sf::st_polygon() %>%
          sf::st_sfc(., crs = my_crs)

  return(bbox)
}


#' Build a vector polygon fishnet from reference object bounds and user-defined resolution.
#'
#' Build a global vector polygon fishnet with grid spacing provided by the user and
#' bounds determined by the input reference object (ref_obj). The data frame is given
#' a unique attribute value for each cell named "fn_key" and is a large integer
#' from 1..n.  The "grid_area" field is the area in the linear units of the user-defined
#' coordinate system for each cell.
#'
#' @param ref_obj An sf spatial object that will be used to create the bounds of the fishnet
#' @param resolution float. The desired grid resolution of the fishnet.
#' @param to_crs integer. The EPSG number of the desired output coordinate
#' reference system. The default is NULL; which will inherit the CRS of the input ref_obj.
#' @return A simple features (sf) spatial data frame object.
#' @export
build_fishnet <- function(ref_obj, resolution, to_crs = NULL) {

  # get the CRS of the input reference spatial data
  native_crs <- sf::st_crs(ref_obj)

  # create grid and give it a fn_key from 1..n and transform to target CRS
  fn <- sf::st_make_grid(ref_obj, cellsize = c(resolution, resolution), crs = native_crs, what = 'polygons') %>%
        sf::st_sf('geometry' = ., data.frame('fn_key' = 1:length(.)))

  # transform if desired
  if (!is.null(to_crs)) {
        fn <- sf::st_transform(fn, crs = to_crs)
  }

  # add grid area field
  fn$grid_area <- sf::st_area(fn$geometry)

  return(fn)
}
