## Team Puma
## Jason Davis & Lieven Slenders. 
## January 13-1-2015
## Lesson 7


## Import municipality mask & Apply CRS 
# for country =NLD ;level of detail=2
municipality_mask <- spTransform(raster::getData('GADM',country= "NLD", level= 2), CRS(proj4string(MODIS)))

