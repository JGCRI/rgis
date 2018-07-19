library(dplyr)
library(sf)

#' Create fractional area of NetCDF grid cells from zones and vice versa
#'
#' Create fractional area of NetCDF grid cells from zones and vice versa. This function
#' creates an output CSV file with attributes for each ...
#'
#' @param poly_path character. A full path with file name and extension to the input polygon shapefile
#' @param ncdf_path character. A full path with file name and extension to the input NetCDF file
#' @param out_csv character. A full path with file name and extension for the output CSV file
#' @export
grid_to_zone_fraction <- function(poly_path, ncdf_path, out_csv, to_crs = 3857) {

  # read in the NetCDF file as a raster brick
  nc <- rgis::import_ncdf_to_raster(ncdf_path)

  # get coordinate system from NetCDF
  proc_crs <- sf::st_crs(raster::projection(nc))

  # get resolution from NetCDF
  resolution <- raster::res(nc)[1]

  # read in county shapefile to sf object and transform projection to NetCDF CRS
  polys <- rgis::import_shapefile(poly_path) %>%
           sf::st_transform(crs = proc_crs)

  # build fishnet from poly extent with grid size of NetCDF and transform output to target projected CRS
  fishnet <- rgis::build_fishnet(polys, resolution = resolution, to_crs = to_crs) %>%
             sf::st_cast("MULTIPOLYGON")

  # get centriods of the fishnet grid cells and transform to CRS of the NetCDF file
  fn_pts <- sf::st_centroid(fishnet) %>%
            sf::st_transform(sf::st_crs(raster::projection(nc)))

  # extract the values of the NetCDF file to a matrix based upon the points; bind to the fishnet centroids
  nc_pts <- raster::extract(nc, fn_pts) %>%
            cbind(fn_pts) %>%
            sf::st_transform(crs = to_crs)

  # transform counties sf object CRS to match fishnet and calculate area field in square meters
  polys <- sf::st_transform(polys, crs = to_crs) %>%
           rgis::add_area_field(field_name = 'poly_area') %>%
           sf::st_cast("MULTIPOLYGON")

  # intersect the polygon zones with the fishnet
  isct <- sf::st_intersection(polys, fishnet) %>%
          sf::st_cast("MULTIPOLYGON")
  isct$part_area <- sf::st_area(isct$geometry)

  # calculate the fraction of the zone that is in a grid cell
  isct$zone_frac <- isct$part_area / isct$poly_area_sqm

  # calculate the fraction of the grid cell that is in a zone
  isct$cell_frac <- isct$part_area / isct$grid_area

  # left join the netcdf points to the fractional features
  nc_pts$geometry <- NULL
  isct$geometry <- NULL
  isfn <- dplyr::left_join(x = isct, y = nc_pts, by = 'fn_key')

  # export as CSV
  write.csv(isfn, file = out_csv, row.names = FALSE)

  return(isfn)
}


