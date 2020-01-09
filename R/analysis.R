#' Analysis tools for rgis


#' Create an altered bounding box from an input sf polygon object
#'
#' @param ply An sf polygon object
#' @return A bounding box keyed list with altered coordinates
#' @importFrom sf st_bbox
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
init_bbox <- function(ply) {
  
  bb <- st_bbox(ply)
  delta <- (bb$ymax[[1]] - bb$ymin[[1]]) / 10
  xmin <- bb$xmin[[1]] - delta
  ymin <- bb$ymin[[1]] - delta
  ymax <- bb$ymax[[1]] + delta
  xmin_source <- bb$xmin[[1]]
  xmax_source <- bb$xmax[[1]]
  
  poly_box <- list()
  poly_box[[ "xmin" ]] <- xmin
  poly_box[[ "ymin" ]] <- ymin
  poly_box[[ "ymax" ]] <- ymax
  poly_box[[ "xmin_source" ]] <- xmin_source
  poly_box[[ "xmax_source" ]] <- xmax_source
  
  return(poly_box)
}


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
#' @importFrom sf st_polygon st_sfc
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
polygon_bounding_box <- function(x_min, x_max, y_min, y_max, my_crs) {

  bbox <- matrix(c(x_min, y_max,
                   x_max, y_max,
                   x_max, y_min,
                   x_min, y_min,
                   x_min, y_max), byrow = TRUE, ncol = 2) %>%
          list() %>%
          st_polygon() %>%
          st_sfc(., crs = my_crs)

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
#' @param lower_left_xy numeric of length 2. lower left corner corrdinates (x, y) of the grid
#' @param to_crs integer. The EPSG number of the desired output coordinate
#' reference system. The default is NULL; which will inherit the CRS of the input ref_obj.
#' @return A simple features (sf) spatial data frame object.
#' @importFrom sf st_crs st_make_grid st_sf st_transform st_area
#' @importFrom raster projection
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
build_fishnet <- function(ref_obj, resolution, lower_left_xy, to_crs = NULL) {

  # get the CRS of the input reference spatial data
  if (class(ref_obj)[1] == "RasterBrick") {
    native_crs <- st_crs(projection(ref_obj))
  }
  else {
    native_crs <- st_crs(ref_obj)
  }

  # create grid and give it a fn_key from 1..n and transform to target CRS
  fn <- st_make_grid(ref_obj, cellsize = c(resolution, resolution), crs = native_crs, offset = lower_left_xy, what = 'polygons') %>%
        st_sf('geometry' = ., data.frame('fn_key' = 1:length(.)))

  # transform if desired
  if (!is.null(to_crs)) {
        fn <- st_transform(fn, crs = to_crs)
  }

  # add grid area field
  fn$grid_area <- st_area(fn$geometry)

  return(fn)
}


#' Create a slice of the original polygon object from a smaller bounding box
#'
#' @param xmin A float or integer value for the x (longitude) coordinate minimum
#' @param xmax A float or integer value for the x (longitude) coordinate maximum
#' @param ymin A float or integer value for the y (latitude) coordinate minimum
#' @param ymax A float or integer value for the y (latitude) coordinate maximum
#' @param ply An sf polygon object
#' @return An sf polygon object
#' @importFrom sf st_polygon st_as_sf st_set_crs st_crs st_intersection st_geometry
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
poly_intersect <- function(xmin, xmax, ymin, ymax, ply) {
  
  return (matrix(c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin), ncol = 2, byrow = TRUE) %>%
            list() %>%
            st_polygon() %>%
            as('Spatial') %>%
            st_as_sf() %>%
            st_set_crs(st_crs(ply)) %>%
            st_intersection(st_geometry(ply)))
}


#' Calculate the area of a slice of a polygon as numeric
#'
#' @param xmax A float or integer value for the x (longitude) coordinate maximum
#' @param coords A list containing xmin, ymin, and ymax
#' @param ply An sf polygon object
#' @return numeric area of a polygon slice
#' @importFrom sf st_area
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
chopped_area <- function(xmax, coords, ply) {
  
  # create polygon bounds list for slice
  poly_slice <- poly_intersect(coords$xmin, xmax, coords$ymin, coords$ymax, ply)
  
  # get slice area from the interescted source polygon
  part_area <- st_area(poly_slice) %>% as.numeric()
  
  return(part_area)
}


#' Calculate the area of the intersected polygon portion
#'
#' @param fraction The fraction of area being processed represented from 0.0 to 1.0
#' @param xmax A float or integer value for the x (longitude) coordinate maximum
#' @param coords A list containing xmin, ymin, and ymax
#' @param ply An sf polygon object
#' @param total_area total area of the whole polygon
#' @return the area of the polygon portion
#' @importFrom sf st_area
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
calulate_polygon_part_area <- function(fraction, xmax, coords, ply, total_area) {
  
  target = total_area * fraction
  
  alt_area <- chopped_area(xmax, coords, ply) - target
  
  return(alt_area)
}


#' Get raster frequency per class from polygon input areas
#'
#' Get the frequency per class of raster values represented in the input raster dataset
#' when restricted to the input watershed polygons for a target city.  This ignores NA.
#'
#' @param raster_object character. An object of class RasterLayer.
#' @param polygon character. A polygon to define spatial boundary of raster value counts (e.g. a given city's watersheds)
#' @return table of crop types present and their frequency of occurrence
#' @importFrom sf st_crs st_transform
#' @importFrom raster crop projection mask unique freq
#' @importFrom tibble as_tibble
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
get_raster_val_classes <- function(raster_object, polygon) {
  
  # transform polygon to sf object if not already
  if(class(polygon)[[1]] != "sf") polygon <- st_as_sf(polygon)
  
  # get the coordinate system of the input raster
  r_crs <- st_crs(projection(raster_object))
  
  # read in shapefile and transform projection to the raster CRS
  polys <- polygon %>%
    st_transform(crs = r_crs)
  
  # calculate the frequency of unique land classes from the input raster that are in the target polygons
  n_lcs <- crop(raster_object, polys) %>%
    mask(polys) %>%
    freq(useNA = "no") %>%
    as_tibble() %>%
    rename(Group.1 = value, x = count)
  
  return(n_lcs)
}


#' Create next polygon slice in sequence
#'
#' @param ply An sf polygon object
#' @param xmax A float or integer value for the x (longitude) coordinate maximum
#' @param xmin A float or integer value for the x (longitude) coordinate minimum
#' @return new polygon slice that is next in sequence
#' @importFrom sf st_bbox
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
slicer <- function(ply, xmin, xmax){
  
  bb = st_bbox(ply)
  delta <- (bb$ymax[[1]] - bb$ymin[[1]]) / 10
  ymin <- bb$ymin[[1]] - delta
  ymax <- bb$ymax[[1]] + delta
  
  r = poly_intersect(xmin[[1]], xmax[[1]], ymin, ymax, ply)
  
  return(r)
}


#' Get the landclasses intersecting a source polygon for large polygons intersecting a
#' high-resolution raster.
#'
#' @param ply An sf polygon object
#' @param rast A raster object
#' @param n_parts integer. The number of equal area parts to split the polygon into for processing.
#' @return new polygon slice that is next in sequence
#' @importFrom sf st_area st_transform st_crs st_as_sf
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @description This function is to be used on very large polygons that would not generally
#' run well when harvesting the information contained in the source high-resolution raster.
#' The source polygon is split into a user-defined number of equal parts and then each of
#' those parts are used to extract the land classes from the source raster in parallel.
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
get_raster_val_classes_byslice <- function(ply, rast, n_parts, n_cores) {
  
  # generate the number of slices based on equal fractions of the total
  fractions <- seq(1/n_parts, (n_parts-1)/n_parts, 1/n_parts)
  
  ply <- st_as_sf(ply) %>%
    st_transform(st_crs(rast)) %>%
    fill_polygon_holes(sqkm_threshold = 10000)
  
  # create initial bounding box as coordinate pair list with proxy xmax
  coords <- init_bbox(ply)
  
  # get the total area of the input polygon
  total_area = st_area(ply) %>%
    as.numeric()
  
  edges <- lapply(fractions, function(fraction){
    
    uniroot(function(x) rgis::calulate_polygon_part_area(fraction, x, coords, ply, total_area),
            lower=coords$xmin_source, upper=coords$xmax_source)$root
  })
  
  xdelta = (coords$xmax_source - coords$xmin_source) / 10
  
  slice_matrix = matrix(c(coords$xmin_source - xdelta, rep(edges,rep(2, length(edges))),
                          coords$xmax_source + xdelta), ncol=2, byrow=TRUE)
  
  slices <- apply(slice_matrix, 1, function(edges){
    slicer(ply, edges[1], edges[2])
  })
  
  # setup parallel backend to use many processors
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  xrd_unagg <- foreach(i = 1:length(slices),
                       .combine=rbind,
                       .packages='teleconnect') %dopar% {
                         tempmat = rgis::get_raster_val_classes(rast, slices[[i]]) #calling a function
                         
                         tempmat #Equivalent to tempmat = cbind(xrd, tempmat)
                       }
  
  aggregate(x = xrd_unagg$x, by = list(xrd_unagg$Group.1), FUN = sum) -> xrd
  
  stopCluster(cl)
  
  return(xrd)
}


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
#' @importFrom sf st_polygon st_sfc
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
build_polygon <- function(xmin, xmax, ymin, ymax) {
  
  return(matrix(c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin), ncol = 2, byrow = TRUE))
}


#' Mask raster to polygon
#'
#' @details masks a raster file against a chosen polygon.
#' @param raster_object character. An object of class RasterLayer.
#' @param polygon character. A polygon to define spatial boundary of raster value counts (e.g. a given city's watersheds)
#' @importFrom sf st_crs st_transform st_as_sf
#' @importFrom raster crop projection mask unique
#' @author Sean Turner (sean.turner@pnnl.gov)
#' @export
mask_raster_to_polygon <- function(raster_object, polygon) {
  
  # transform polygon to sf object if not already
  if(class(polygon)[[1]] != "sf") polygon <- st_as_sf(polygon)
  
  # get the coordinate system of the input raster
  r_crs <- st_crs(projection(raster_object))
  
  # read in shapefile and transform projection to the raster CRS
  polys <- polygon %>%
    st_transform(crs = r_crs)
  
  # crop and mask
  n_lcs <- crop(raster_object, polys) %>%
    mask(polys)
  
  return(n_lcs)
}

