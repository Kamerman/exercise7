## NL greenest city
## Team Puma
## Jason Davis & Lieven Slenders. 
## January 12-1-2015
## Lesson 7
# -------------------------------------------------------


##Download data

downloadData <- function(file_url, dest) {
  library(downloader)
  x <- download.file(url = file_url , destfile = dest, mode = 'wb')
}





# download.file(url = "https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip", 
#               destfile = "data/modis.zip",
#               quiet = T,
#               mode = 'wb')
