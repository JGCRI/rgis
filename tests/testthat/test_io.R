Context("Test io functions")

test_that("Test import shapefile", {
  #Import the shape file
  expect_silent(sf <- import_shapefile("test_data/shp/northcarolina_county_wgs84.shp", method= "sf"))
  expect_silent(sf_rgdal <- import_shapefile("test_data/shp/northcarolina_county_wgs84.shp", method= "rgdal"))
  
  expect(typeof(sf)=="list","Return type of import_shapefile is not a list ")
  expect(typeof(sf_rgdal)=="S4","Return type of import_shapefile is not a spatial polygons dataframe")
})

test_that("Test import raster", {
  #Import the raster
  expect_silent(raster <- import_raster("test_data/tif/tas_watch_jan_1991_wgs84.tif"))
  
  
  expect_silent(as.data.frame(rasterToPoints(raster)),"Raster is invalid. Unable to convert raster to data frame")
  expect_error(plot(raster),NA,"Raster is invalid. Unable to plot raster.")
})

test_that("Test import ncdf", {
  #Import the raster
  expect_silent(raster <- import_ncdf_to_raster("test_data/nc/tas_watch_monthly_1991.nc4"))
  #Test type
  expect(typeof(raster)=="S4","ncdf is not being translated to raster correctly")
  #Convert to dataframe
  expect_silent(as.data.frame(raster,xy=TRUE),"Raster is invalid. Unable to convert raster to data frame")
  #Test that raster can be plotted
  expect_error(plot(raster),NA,"Raster is invalid. Unable to plot raster.")
})