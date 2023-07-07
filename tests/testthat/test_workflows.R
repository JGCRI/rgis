context("test workflow functions")
library(raster)

test_that("Test intersection fraction computation", {
  
  expect_warning(shp_tmp <- import_shapefile("test_data/shp/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp"))
  
  expect_warning(crs_tmp <- st_crs(projection(shp_tmp)))
  
  expect_warning(test_output <- get_intersection_fractions(out_csv = NULL,out_shape_file = NULL,shpfile_1 = "test_data/shp/tl_2019_us_state.shp",
                                            shpfile_2 = "test_data/shp/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp",
                                            default_crs = crs_tmp,
                                            write_other_files=FALSE))
  
  tmpna <- test_output[is.na(test_output$ratio_1),]
  tmpna2 <- test_output[is.na(test_output$ratio_2),]
  
  expect(nrow(tmpna)==0 & nrow(tmpna2)==0,"NA values being returned by intersection fractions function")
})

test_that("Test zone fractions computation", {
  #Import the raster
  expect_warning(test_output <- grid_to_zone_fractions(poly_path = "test_data/shp/northcarolina_county_wgs84.shp",
                                                      raster_path = "test_data/nc/tas_watch_monthly_1991.nc4",csv_name ="tmp.csv",
                                                      perform_check = FALSE))
  test_output %>% 
    group_by(GEOID) %>% 
    mutate(zone_frac=sum(as.double(zone_frac)),cell_frac=sum(as.double(zone_frac))) %>% 
    ungroup() %>% 
    dplyr::select(GEOID,zone_frac) %>% 
    distinct()->grouped_output
  
  tmp <- grouped_output %>% filter(zone_frac < 0.96)
  
  expect(nrow(tmp)==0 ,"Zone fractions not adding up to 1")
  
})

