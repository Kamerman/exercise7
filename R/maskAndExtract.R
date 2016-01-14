## NL greenest city
## Team Puma
## Jason Davis & Lieven Slenders. 
## January 12-1-2015
## Lesson 7


## Mask raster data to Netherlands -------------------------------------------------------
library(raster)
library(rgdal)

wagContourUTM <- spTransform(wagContour, CRS(proj4string(wagLandsat)))
  

maskAndExtract <- function(country, lvl, input_raster) { 
  municipality_mask <- raster::getData('GADM',country= country, level= lvl)
  extract(input_raster, municipality_mask) -> x
  return(x)
  }

# Investigate the structure of the object

NL <- maskAndExtract('NLD', 2, MODIS)



 