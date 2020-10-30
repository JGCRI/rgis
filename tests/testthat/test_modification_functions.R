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
