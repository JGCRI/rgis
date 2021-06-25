context("Test modification functions")
library(sf)

test_that("Test area calculations", {
  #Import the shapefile
  expect_silent(sf <- import_shapefile("test_data/shp/northcarolina_county_wgs84.shp"))
  
  #Expect that shapefile is read in correctly
  tmp <- st_is_empty(sf)
  
  expect_silent(sf %>% 
    add_area_field("calc_area")->sf)
  
  expect(typeof(sf$calc_area)=="double","Areas are not being calculated correctly")
  
})

test_that("Test hole filling functions", {
  
  #Import the shapefile
  expect_silent(sf <- import_shapefile("test_data/shp/northcarolina_county_wgs84.shp"))
  
  
  
  expect_silent(sf %>% fill_polygon_holes(sqkm_threshold=10000)-> sf_hole_filled)
  
  exceptions <- st_is_valid(sf_hole_filled, NA_on_exception = TRUE)
  na_exceptions <- exceptions[is.na(exceptions)]
  self_intersections <- exceptions[exceptions=="FALSE"]
  
  expect(length(na_exceptions)==0,"Invalid geometries are being returned after hole filling")
  expect(length(self_intersections)==0,"Self intersections are being returned after hole filling")
  
})