library('devtools')
library('rgis')

# load example data set
data_ncdf <- rgis::import_ncdf_to_raster(ncdf_file = 'data-raw/tas_watch_monthly_1991.nc4')
data_shp <- rgis::import_shapefile(shp_path = 'data-raw/northcarolina_county_wgs84.shp')
data_tif <- rgis::import_raster(raster_path = 'data-raw/tas_watch_jan_1991_wgs84.tif')

devtools::use_data(data_ncdf, data_shp, data_tif, internal = TRUE, overwrite = TRUE)