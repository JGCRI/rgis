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
grid_to_zone_fractions <- function(poly_path = "C:/Projects/ctry_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp",
                                   raster_path = "C:/Projects/annual_area_harvested_irc_crop01_ha_30mn.asc",
                                   csv_name = "MIRCA_intersections.csv",
                                   perform_check=TRUE){
  
  extent= c(-180,180,-90,90)
  sf_use_s2(FALSE)
  
  r <- raster(raster_path)
  
  resolution <- res(r)[1]
  
  
  
  
  polys <- rgis::import_shapefile(poly_path) %>% 
    add_area_field("polygon_area")
  
  gr <- st_make_grid(polys, cellsize = c(resolution,resolution)) %>% 
    st_sf()%>% 
    add_area_field("cell_area") %>% 
    mutate(cell_id=row_number())
  
  grid_lonlat <- st_transform(gr, "+proj=longlat")
  centroids <- st_centroid(grid_lonlat)
  
  gr$Latitude <- st_coordinates(centroids)[, 2]
  gr$Longitude <- st_coordinates(centroids)[, 1]
  
  
  
  gr %>% st_make_valid() %>% filter(st_is(. , c("POLYGON", "MULTIPOLYGON")))->valid_grid
  
  
  
  isct <- st_intersection(st_make_valid(polys), valid_grid)
  isct$part_area <- st_area(isct$geometry)
  isct$zone_frac <- isct$part_area / isct$polygon_area
  isct$cell_frac <- isct$part_area / isct$cell_area
  
  isct <- as.data.frame(isct)
  isct %>% dplyr::select(-geometry)->isct
  
  if(!is.null(csv_name)){
    write.csv(isct, csv_name,row.names = FALSE)
  }
  
  
  if(perform_check){
    raster_check <- as.data.frame(rasterToPoints(raster(raster_path)))
    
    colnames(raster_check)<- c("x","y","rast_val")
    
    isct %>% 
     mutate(Longitude=round(Longitude,2),Latitude=round(Latitude,2))%>% 
     left_join(raster_check, by = c("Longitude"="x","Latitude"="y")) %>% 
     mutate(new_val=rast_val*cell_frac)->t_file
    
    t_file <- as.data.frame(t_file)
    
    print(paste0("Total value from original raster is " ,sum(raster_check$rast_val)))
    print(paste0("Total value from revised cell fractions is " ,sum(t_file$new_val)))
    
    
  }
  
  return(isct)
}

#' Return intersection areas of two spatial files along with proportions used to perform area based downscaling/aggregations 
#'
#' Intersect two shape files/spatial files, return proportions used to downscale or aggregate data based on calculated areas. 
#' The function will return a sf data frame with additional columns including the areas for the spatial boundaries
#' and the intersection. Two ratios are returned which can be used to downscale data or aggregate data as required.  
#'
#' @param shpfile_1 character. A full path with file name and extension to the input polygon shapefile
#' @param shpfile_2 character. A full path with file name and extension to the input polygon shapefile 
#' @param area_field_1 character.Name of column with area calculated for shapefile 1
#' @param area_field_2 character.Name of column with area calculated for shapefile 2   
#' @param out_csv character. A full path with file name and extension for the output CSV file
#' @param to_crs int. The EPSG number of a projected coordinate reference system.  Must be projected
#' and not geographic due to assumptions that sf::st_intersection makes when using
#' geographic systems.  Default is EPSG:4326, WGS 84 
#' @param shape_file character. The name of the shapefile to be returned 
#' @param shape_file_cols vector. The names to be selected when returning the shape file.
#' @return data.frame
#' @importFrom sf st_crs st_transform st_bbox st_cast st_centroid st_write st_intersection st_area 
#' @importFrom raster res projection extract
#' @importFrom dplyr left_join filter 
#' @author Kanishka Narayan (kanishka.narayan@pnnl.gov)
#' @export
get_intersection_fractions <- function(shpfile_1= "temporary_output/tl_2019_us_state.shp",
                                       shpfile_2= "temporary_output/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp",
                                       area_field_1 = "state_area",
                                       area_field_2 = "basin_area",
                                       default_crs = 4326,
                                       out_csv =NULL,
                                       out_shape_file ="gcamusa_state_glu_intersections.shp",
                                       shape_file_cols =c("glu_id","NAME","GEOID"),
                                       write_other_files=FALSE){
  
  shp1_st <- st_read(shpfile_1)
  
  if(is.null(default_crs)){
    
    default_crs <- crs(shp1_st)  
  
  }
  
  
  
  
  shp1_st %>% st_transform(crs=default_crs) %>% 
              st_cast("MULTIPOLYGON") %>%
              st_make_valid() %>% 
              add_area_field(area_field_1) -> shp1_st
  
  bbox1 <- st_bbox(shp1_st)
  
  shp2_st <- st_read(shpfile_2) %>%
             st_transform(crs=default_crs) %>% 
             st_cast("MULTIPOLYGON") %>% 
             st_make_valid() %>% 
            add_area_field(area_field_2)   -> shp2_st
  
  bbox2 <- st_bbox(shp2_st)
  
  if((bbox1[3]>bbox2[3]) & (bbox1[4]>bbox2[4])){
    
    
    default_bbox <- bbox2 
    
  }else{
    
    default_bbox <- bbox1
  }
  
  shp1_st <- st_crop(shp1_st, default_bbox)
  
  shp2_st <- st_crop(shp2_st, default_bbox)
  
  
  st_make_valid(shp1_st) %>% st_intersection(st_make_valid(shp2_st)) -> insct                      
  
  insct %>% filter(st_is(. , c("POLYGON", "MULTIPOLYGON"))) -> insct
  
  insct %>% st_cast("MULTIPOLYGON") -> insct 
  
  insct %>% add_area_field("intersection_area") %>% 
            mutate(ratio_1 = intersection_area/get(area_field_1),
                   ratio_2 = intersection_area/get(area_field_2))-> insct_data
  
  if(!is.null(out_csv)){
    
    write.csv(as.data.frame(insct_data) %>% select(-geometry),out_csv,row.names = FALSE)
    
  }
  
  if(!is.null(out_shape_file)){
    
    shape_file_cols <- c(shape_file_cols,"geometry")
    
    insct_data %>% select(shape_file_cols)->insct_data
    
    sf::st_write(insct_data,out_shape_file,driver="ESRI Shapefile")
    
  }
  
  if(write_other_files){
    
    sf::st_write(shp1_st,"shp1.shp",driver="ESRI Shapefile")
    sf::st_write(shp2_st,"shp2.shp",driver="ESRI Shapefile")
    
  }
  
  
  insct_data <- st_as_sf(insct_data)
  
  return(insct_data)    
  
}

