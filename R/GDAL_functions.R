#GDAL functions

#' reproject_raster
#'
#' Convert raster to specific dimensions and formats
#'
#' @param inupt_file location of input file
#' @param input_format format of the input file  
#' @param method Method of conversion. Default is set to average 
#' @param projection Output projection type. Defualt is set to +proj=longlat +ellps=WGS84
#' @param col_row columns and rows in output dataset. Defualt is set to c(4320,2160) 
#' @param extent output extent. Default is set to c(-180,-90,180,90)
#' @param output_format Format of output. Currently set to ENVI.
#' @param out_file Name of output file
#' @importFrom gdalUtilities gdalwarp gdal_translate
#' @author Kanishka Narayan (kanishka.narayan@pnnl.gov)
#' @export
reproject_raster<- function(input_file='input.tif',input_format=".tif",NO_DATA_STATUS=TRUE,method="average", no_data_value=127,
                            resolution="+proj=longlat +ellps=WGS84",col_row=c(4320,2160),
                            extent=c(-180,-90,180,90),out_file="outfile",output_format="ENVI"){
  
  return(gdalwarp(input_file,out_file,r= method,
                  ot="Float32",t_srs=resolution, te= extent,ts=col_row, of=output_format, dstnodata = 0, a_nodata ="none"))
  
}

#' convert_file
#'
#' Convert files to specific formats
#'
#' @param inupt_file location of input file
#' @param output_format Format of output. Currently set to ENVI.
#' @param out_file Name of output file
#' @importFrom gdalUtilities gdalwarp gdal_translate
#' @author Kanishka Narayan (kanishka.narayan@pnnl.gov)
#' @export
convert_file <- function(inupt_file="input.tif",out_file="output.bil",output_format="ENVI"){
  
  
  return(gdal_translate(inupt_file,out_file, of = output_format))
  
  
}

