# Conversion tools for rgis

library(fasterize)
library(raster)
library(spex)
library(sf)
library(ncdf4)


#' Create a raster from a polygon sf object
#'
#' Creates a raster from a polygon sf object using the fasterize library and
#' function which provides great improvement over raster::rasterize. See
#' https://cran.r-project.org/web/packages/fasterize/fasterize.pdf
#'
#' @param sf an sf::sf() object with a geometry column of POLYGON and/or MULTIPOLYGON
#' objects
#' @param raster A raster object. Used as a template for the raster output. Can be created with
#' raster::raster()
#' @param field character. The name of a column in sf, providing a value for each of the polygons
#' rasterized. If NULL (default), all polygons will be given a value of 1
#' @param fun character. The name of a function by which to combine overlapping polygons.
#' Currently takes "sum", "first", "last", "min", "max", "count", or "any". Future
#' versions may include more functions or the ability to pass custom R/C++ functions.
#' If you need to summarize by a different function, useby= to get a RasterBrick
#' and then raster::stackApply() or raster::calc() to summarize
#' @param background numeric. Value to put in the cells that are not covered by any of the features of
#' x. Default is NA.
#' @param by character. The name of a column in sf by which to aggregate layers. If set,
#' fasterize will return a RasterBrick with as many layers as unique values of the by column
#' @importFrom fasterize fasterize
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @return raster object
#' @export
polygon_to_raster <- function(sf, raster, field = NULL, fun, background = NA_real_, by = NULL) {
  
  return(fasterize(sf, raster, field, fun, background, by))
}


#' Create a polygon sf object from a raster
#'
#' Create a polygon sf object from a raster, stack, or brick.
#'
#' @param raster A raster, stack, or brick object
#' @param na.rm boolean. If TRUE will polygonize only non-NA cells. Defualt is FALSE.
#' @importFrom spex qm_rasterToPolygons
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
raster_to_polygon <- function(raster, na.rm = FALSE) {
  
  return(qm_rasterToPolygons(raster, na.rm))
}


#' Convert NetCDF file to a CSV
#'
#' Convert NetCDF file to a CSV
#'
#' @param ncdf_file character. asdf
#' @param out_csv character. asdf
#' @param nc_var_name character. asdf
#' @param resolution float. asdf
#' @param start_year int
#' @param through_year int
#' @param nmonths int
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @author Chris Vernon (chris.vernon@pnnl.gov)
#' @export
ncdf_to_csv <- function(ncdf_file, out_csv, nc_var_name, resolution = 0.5, start_year = NULL, through_year = NULL, nmonths = NULL) {
  
  n <- nc_open(ncdf_file)
  cell_vals <- ncvar_get(n, nc_var_name)
  nc_close(n)
  
  lon_ext = dim(cell_vals)[1]
  lat_ext = dim(cell_vals)[2]
  
  # use all months in file if no setting
  if (is.null(nmonths) & is.null(start_year) & is.null(through_year)) {
    nmonths <- dim(cell_vals)[3] # Assuming lat/lon/month
  }
  
  # if using start and through years, calc months
  else if (!is.null(start_year) & !is.null(through_year) & is.null(nmonths)) {
    nmonths <- (through_year - start_year) + 1 * 12
  }
  
  # build output matrix
  template <- expand.grid(seq(0, lat_ext-resolution, by = resolution),
                          seq(0, lon_ext-resolution, by = resolution),
                          1:nmonths)
  combined <- cbind(template, as.vector(cell_vals[ , , 1:nmonths]))
  names(combined) <- c('lat', 'lon', 'month', nc_var_name)
  
  return(combined)
}

#' warp_raster
#'
#' Convert raster to specific dimensions and formats
#'
#' @param inupt_file location of input file
#' @param input_format format of the input file  
#' @param NO_DATA_STATUS Boolean for no_data values. Set this to true if there are NO_DATA values in the data set. If there are no data values, this will fill them with 0
#' @param no_data_value Value of no_data in the dataset
#' @param method Method of conversion. Default is set to average 
#' @param projection Output projection type. Defualt is set to +proj=longlat +ellps=WGS84
#' @param col_row columns and rows in output dataset. Defualt is set to c(4320,2160) 
#' @param extent output extent. Default is set to c(-180,-90,180,90)
#' @param output_format Format of output. Currently set to ENVI.
#' @param out_file Name of output file
#' @importFrom gdalUtilities gdalwarp gdal_translate
#' @author Kanishka Narayan (kanishka.narayan@pnnl.gov)
#' @export

warp_raster<- function(input_file='input.tif',input_format=".tif",NO_DATA_STATUS=TRUE,method="average", no_data_value=127,
                       resolution="+proj=longlat +ellps=WGS84",col_row=c(4320,2160),
                       extent=c(-180,-90,180,90),out_file="outfile",output_format="ENVI"){
  
  
  if (NO_DATA_STATUS==TRUE){
    print("Warning: filling no_data values may take a significantly long time  with big datasets")
    
    gdalwarp(srcfile = input_file, dstfile = paste0("tmp",input_format) ,srcnodata = no_data_value,dstnodata = 0)
    
    gdal_translate(a_nodata = 0, src_dataset = paste0("tmp",input_format),dst_dataset = paste0("tmp1",input_format))
    
    file.remove(paste0("tmp",input_format))
    
    #Take off no-data tag
    gdal_translate(a_nodata = "none", src_dataset = paste0("tmp1",input_format),dst_dataset = paste0("tmp2",input_format))
    
    file.remove(paste0("tmp1",input_format))
    
    return(gdalwarp(paste0("tmp2",input_format),out_file,r= method,
             ot="Float32",t_srs=resolution, te= extent,ts=col_row, of=output_format))
    
    file.remove(paste0("tmp2",input_format))
  }else{
  
  return(gdalwarp(input_file,out_file,r= method,
           ot="Float32",t_srs=resolution, te= extent,ts=col_row, of=output_format))
  
}
}

