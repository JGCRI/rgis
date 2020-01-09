library(dplyr)
library(sf)
library(lwgeom)

#' Create fractional area of NetCDF grid cells to corresponding polygon features and vice versa
#'
#' Create fractional area of NetCDF grid cells to corresponding polygon features and
#' vice versa. This function creates an output CSV file with attributes from the input
#' polygon features and their spatially intersecting NetCDF grid cells for each time step
#' in the file. The user can chose to save intermediate step shapefiles as well.  Fields
#' that describe the fraction of a polygon that is in a grid cell (zone_frac) and the
#' fraction of a grid cell that is in a zone (cell_frac) are output in the final CSV.
#'
#' @param poly_path character. A full path with file name and extension to the input polygon shapefile
#' @param ncdf_path character. A full path with file name and extension to the input NetCDF file
#' @param out_csv character. A full path with file name and extension for the output CSV file
#' @param to_crs int. The EPSG number of a projected coordinate reference system.  Must be projected
#' and not geographic due to assumptions that sf::st_intersection makes when using
#' geographic systems.  Default is EPSG:3857, WGS 84 / Pseudo-Mercator - Spherical Mercator used by
#' Google Maps, OpenStreetMap, etc.
#' @param save_isct character. A full path with file name and extension (shp) for the output intersecting
#' polygons to fishnet grid shapefile if desired as an output. Defualt NULL.
#' @param save_fishnet character. A full path with file name and extension (shp) for the output fishnet grid
#' shapefile if desired as an output. Defualt NULL.
#' @param save_ncpts character. A full path with file name and extension (shp) for the output NetCDF grid
#' centroids that intersect the polygon fishnet shapefile if desired as an output. Defualt NULL.
#' @param filter_na boolean. TRUE (default) to remove NA land values where a fishnet grid intersected
#' a polygon but the NetCDF had no underlying value; FALSE to keep NA
#' @return data.frame
#' @importFrom sf st_crs st_transform st_bbox st_cast st_centroid st_write st_intersection st_area 
#' @importFrom raster res projection extract
#' @importFrom dplyr left_join filter 
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
grid_to_zone_fraction <- function(poly_path, ncdf_path, out_csv, to_crs = 3857, save_isct = NULL,
                                  save_fishnet = NULL, save_ncpts = NULL, filter_na = TRUE) {

  # read in the NetCDF file as a raster brick
  nc <- rgis::import_ncdf_to_raster(ncdf_path)

  # get coordinate system from NetCDF
  proc_crs <- st_crs(projection(nc))

  # get resolution from NetCDF
  resolution <- res(nc)[1]

  # read in county shapefile to sf object and transform projection to NetCDF CRS
  polys <- rgis::import_shapefile(poly_path) %>%
           st_transform(crs = proc_crs)

  poly_bbox <- st_bbox(polys)
  llc <- c(floor(poly_bbox[1]), floor(poly_bbox[2]))

  # build fishnet from poly extent with grid size of NetCDF and transform output to target projected CRS
  fishnet <- rgis::build_fishnet(polys, resolution = resolution, lower_left_xy = llc, to_crs = to_crs) %>%
             st_cast("MULTIPOLYGON")

  # get centriods of the fishnet grid cells and transform to CRS of the NetCDF file
  fn_pts <- st_centroid(fishnet) %>%
            st_transform(st_crs(projection(nc)))

  # extract the values of the NetCDF file to a matrix based upon the points; bind to the fishnet centroids
  nc_pts <- extract(nc, fn_pts) %>%
            cbind(fn_pts) %>%
            st_transform(crs = to_crs)

  if (!is.null(save_ncpts)) {
    st_write(nc_pts, save_ncpts)
  }

  # transform counties sf object CRS to match fishnet and calculate area field in square meters
  polys <- st_transform(polys, crs = to_crs) %>%
           rgis::add_area_field(field_name = 'poly_area') %>%
           st_cast("MULTIPOLYGON")

  if (!is.null(save_fishnet)) {
    st_write(fishnet, save_fishnet)
  }

  # intersect the polygon zones with the fishnet
  isct <- st_intersection(polys, fishnet) %>%
          st_cast("MULTIPOLYGON")
  isct$part_area <- st_area(isct$geometry)

  # calculate the fraction of the zone that is in a grid cell
  isct$zone_frac <- isct$part_area / isct$poly_area

  # calculate the fraction of the grid cell that is in a zone
  isct$cell_frac <- isct$part_area / isct$grid_area

  if (!is.null(save_isct)) {
    st_write(isct, save_isct)
  }

  # left join the netcdf points to the fractional features
  nc_pts$geometry <- NULL
  isct$geometry <- NULL
  isfn <- left_join(x = isct, y = nc_pts, by = 'fn_key')

  # remove NA land values where a fishnet grid intersected a polygon but the NetCDF had no underlying value
  if (filter_na == TRUE) {
    isfn <- filter(isfn, !is.na(isfn[names(nc_pts)[1]]))
  }

  # drop unneeded columns
  drops <- c('poly_area', 'grid_area.x', 'part_area', 'grid_area.y')
  isfn <- isfn[ , !(names(isfn) %in% drops)]

  # export as CSV
  write.csv(isfn, file = out_csv, row.names = FALSE)

  return(isfn)
}
