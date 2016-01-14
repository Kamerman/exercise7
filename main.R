# Luc van Zandbrink 
# 12/01/2016
# exercise7

# install packages
library(sp)
library(raster)
library(rgdal)
library(downloader)

# load function sources

# download and unzip MODIS data
download.file("https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip", "data/MODIS", mode = 'wb')
source('R/downloadData.R')
downloadData("https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip", "data/MODIS")
source('R/unzipFile.R')
unzipFile('data/MODIS', 'data')
# Reading the grd file
library(sp)
library(raster)
  MODIS <- stack("data/MOD13A3.A2014001.h18v03.005.grd") # returns: MODIS

# import municipalities
source('R/import_municipalities.R') # returns : municipality_mask

# # Calculations ---------------------------------
# Extracting NDVI; calculate mean; for defined mask 
NDVI_NL <- extract(MODIS, 
                   municipality_mask,
                   sp=TRUE,
                   df=TRUE,
                   fun=mean) ## df=TRUE i.e. return as a data.frame

# Define dataframe as variable to make easier to work with
df <- NDVI_NL@data # has municip name as " df$NAME_2"  ; NDVI as " month name" 
  
# Add column for yearly average of NDVI values to dataframe
df$yearMean <- rowMeans(df[16:27], na.rm = TRUE)

  
## function to take max value of NDVI for a given time period --------------------------------------------------------------------------------

returngreenestcity <- function(month_number, plot_image) { 
  month_number <- month_number + 15
  greenestCity <- df[which(df[month_number] == max(df[month_number], na.rm = TRUE)), ]$NAME_2
  
  greenestValue <- df[which(df[month_number] == max(df[month_number], na.rm = TRUE)), ][month_number]

  if(plot_image == T){ 
    subset <- subset(NDVI_NL, df$NAME_2 == greenestCity, drop = F) 
    plot(main = paste(as.character(greenestCity)),
                    municipality_mask)
    plot(subset, col="red", add=T)
    return(c(greenestCity,greenestValue))
    }
  return(c(greenestCity,greenestValue))
}

## Instructions: run function to find greenest city
  # month_number must be a value between 1=January, 2= February, etc.
  # 13 = whole year average
  # plot image must be T/F if user would like to plot
returngreenestcity(month_number, plot_image)







