#library(terra)
library(raster)
library(sf)
library(exactextractr)
library(rgdal)


dirctry_data <- "F:/coe1000/melanie/indicators"
output_data <- "F:/coe1000/melanie/stratificati_R"

LC <- raster("F:\\coe1000\\melanie\\indicators\\LC\\LC.tif")
#crs(LC) <- sp::CRS("EPSG:3035")
#sp::CRS("EPSG:3035")
#crs(LC)<-CRS("+init=epsg:3035")


issue <- raster("F:\\coe1000\\melanie\\indicators\\POPULATION\\pop_250m.tif")

#LC_shp <- st_read("F:\\coe1000\\melanie\\indicators\\LC\\LC_dissolved.shp")
#
#LC_shp$Median <- exact_extract(builtup, LC_shp, 'median')
#LC_shp$Median
#LC2 = LC_shp|>st_drop_geometry() ## per togliere la geometria


issue <- crop(issue, extent(LC))  #taglia il raster A sull'extent del raster B
res(issue) <- res(LC) 
extent(issue) <- extent(LC)
       

#crs(LC_shp)

s<- issue
r<- LC
result <- zonal(s, r, 'median')
result

## Result population


#reclassify(r, result, file.path(dirctry_data, "LC_reclassified.tif"))

r2 <- reclassify(r, result)

rc <- function(x) {
  
  x1 <- x[1]  ##valore pixel della prima banda
  x2 <- x[2] ##valore pixel 2 banda
  ifelse(x1 <=  x2, 0,
  ifelse(x1 >  x2, 1, NA)) }


s2 <- stack(s,r2)


beginCluster(6) ##ASSIGN CORE
z1 <- clusterR(s2, calc, args=list(fun=rc), filename=file.path(output_data, "population_issue_R.tif"), datatype = "INT1U", overwrite = TRUE)
endCluster()


plot(z1)
z1

#writeRaster(x = z1, paste0(dirctry_data, "/rusleR_R.tif"), format = 'GTiff', overwrite = TRUE)

