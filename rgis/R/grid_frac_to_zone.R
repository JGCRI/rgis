library(dplyr)
library(sf)
library(tools)
library(lwgeom)

#' Build a global vector polygon fishnet with grid spacing provided by the user.
#' The data frame is given a unique attribute value for each cell; this is named
#' "fn_key" and is a large integer from 1..n.  The "grid_area" field is the area in
#' the linear units of the defined coordinate system for each cell.
#'
#' @param resolution A float value for the desired grid resolution of the fishnet.
#' @param my_crs The EPSG number of the desired coordinate reference system. The
#' default is 4326 (WGS 1984).
#' @return A simple features (sf) spatial data frame object.
#' @export
build_fishnet <- function(resolution, my_crs = 4326) {

  # create a global bounding box initially in WGS 84 - EPSG 4326
  bbox <- matrix(c(-180,  90,
                    180,  90,
                    180, -90,
                   -180, -90,
                   -180,  90), byrow = TRUE, ncol = 2) %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc(., crs = 4326)

  # create grid and give it a fn_key from 1..n and transform to target CRS
  fn <- sf::st_make_grid(bbox, cellsize = c(resolution, resolution), crs = 4326, what = 'polygons') %>%
        sf::st_sf('geometry' = ., data.frame('fn_key' = 1:length(.))) %>%
        sf::st_transform(crs = my_crs)

  # add grid area field
  fn$grid_area <- sf::st_area(fn$geometry)

  return(fn)
}

#' Import point data that contains a value to be spatially joined to the fishnet containing
#' fractional area.  May either be a shapefile or a CSV file containing a latitude and longitude
#' for each record.
#'
#' @param f The full path with filename and extension to the points dataset.
#' @param pts_lat_field The field name for latitude if using a CSV to provide point data.
#' @param pts_lon_field The field name for longitude if using a CSV to provide point data.
#' @param pts_crs The native EPSG number for the coordinate reference system used in the
#' creation of the input points data. The default is 4326 (WGS 1984).
#' @param my_crs The EPSG number of the desired coordinate reference system. The
#' default is EPSG:3857 the WGS 84 / Pseudo-Mercator -- Used by all modern web
#' mapping applications.
#' @return A simple features (sf) spatial data frame object.
#' @export
import_points <- function(f, pts_lat_field = FALSE, pts_lon_field = FALSE, pts_crs = 4326,
                          my_crs = 4326) {

  # get file extension
  fext <- tools::file_ext(f) %>%
          tolower()

  if (fext == 'csv') {
    pts <- read.csv(file = f, header = TRUE, sep = ',')

    # change latitude, longitude columns to numeric
    cols.num <- c(lat_field, lon_field)
    pts[cols.num] <- sapply(pts[cols.num], as.numeric)

    # convert to sf spatial data frame object and transform to target CRS
    pts.SP <- sf::st_as_sf(pts, coords = c(lat_field, lon_field), crs = pts_crs) %>%
              sf::st_transform(crs = my_crs)
  }

  else if (fext == 'shp') {
    pts <- sf::st_read(f) %>%
           sf::st_transform(crs = my_crs)
  }

  else {
    pts <- FALSE
  }

  return(pts)
}

#' Create an output point shapefile and CSV file containing attributes for the
#' fraction of the provided polygon zones that are in a grid cell ("zone_frac") and the
#' fraction of the grid cells that are in a polygon zone ("cell_frac). Grid cells are created
#' using a vector polygon fishnet at the user-defined resolution in decimal
#' degrees (e.g., 0.5 for half-degree). NA values are replaced with 0 in the output data frame.
#'
#' @param poly_zone_shp The full path with filename and extension to the input
#' shapefile containing polygon zones.
#' @param points_file The full path with filename and extension to the input
#' points data containing a value per point to assign to the fishnet cells. This
#' file can either be a point shapefile or a CSV file containing a latitude and
#' longitude per record.
#' @param pts_value_field The field name that holds the value from the points data that will
#' be transfered to the output data.
#' @param resolution A float value for the desired grid resolution of the fishnet.
#' @param pts_lat_field The field name for latitude if using a CSV to provide point data.
#' @param pts_lon_field The field name for longitude if using a CSV to provide point data.
#' @param pts_crs The native EPSG number for the coordinate reference system used in the
#' creation of the input points data. The default is 4326 (WGS 1984).
#' @param my_crs The EPSG number of the desired coordinate reference system. The
#' default is EPSG:3857 the WGS 84 / Pseudo-Mercator -- Used by all modern web
#' mapping applications because it is represented in meters. This
#' is the default because sf::st_intersection assumes that the coordinates are projected
#' even if they are in latitude, longitude which may cause incorrect behavior away from
#' the equator.
#' @return A simple features (sf) spatial data frame object containing points, their
#' values, and the fractional information.
#' @export
grid_frac_zone <- function(poly_zone_shp, points_file, pts_value_field, resolution,
                           pts_lat_field = 'latitude', pts_lon_field = 'longitude',
                           pts_crs = 4326, my_crs = 4326) {

  # import polygon zone shapefile and transform it to the target CRS
  polys <- sf::st_read(poly_zone_shp) %>%
           sf::st_transform(crs=my_crs)
  polys$zone_area <- sf::st_area(polys$geometry)

  # import points shapefile and transform it
  points <- import_points(f = points_file,
                          pts_lat_field = pts_lat_field,
                          pts_lon_field = pts_lon_field,
                          pts_crs = pts_crs,
                          my_crs = my_crs)

  # build the fishnet
  fishnet <- build_fishnet(resolution, my_crs)

  # intersect the polygon zones with the fishnet
  isct <- sf::st_intersection(polys, fishnet)
  isct$part_area <- sf::st_area(isct$geometry)

  # calculate the fraction of the zone that is in a grid cell
  isct$zone_frac <- isct$part_area / isct$zone_area

  # calculate the fraction of the grid cell that is in a zone
  isct$cell_frac <- isct$part_area / isct$grid_area

  # intersect the grid-zone features back to the fishnet
  isfn <- sf::st_join(fishnet$geometry, isct, join = sf::st_intersects)

  # import points shapefile and transform it
  points <- import_points(f = points_file,
                          pts_lat_field = pts_lat_field,
                          pts_lon_field = pts_lon_field,
                          pts_crs = pts_crs,
                          my_crs = my_crs)

  # intersect the grid poly parts with the points to inherit attributes
  icpts <- sf::st_intersection(points, isfn)

  # replace NA with 0
  icpts[is.na(icpts)] <- 0

  return(icpts)
  # return(icpts[c('fn_key', 'zone_frac', 'cell_frac', pts_value_field)])
}
