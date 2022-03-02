
library(raster)
library(sf)
library(sp)


dirctry_data <- "F:/coe1000/melanie/indicators"
output_data <- "F:/coe1000/melanie/stratificati_R"

LC <- raster("F:\\coe1000\\melanie\\indicators\\LC\\LC.tif")

##If the CRS is not assigned
crs(LC) <- st_crs(3035)$wkt
## see https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/

LC

#issue <- raster("F:\\coe1000\\melanie\\indicators\\POPULATION\\pop_250m.tif")
#issue <- raster("F:\\coe1000\\melanie\\indicators\\RUSLE\\rusle_input_1000.tif")
#issue <- raster("F:\\coe1000\\melanie\\indicators\\ACIDIFICATION\\eacidification_1000.tif")
#issue <- raster("F:\\coe1000\\melanie\\indicators\\EUTROFICATION\\eutrofication_1000.tif")
#issue <- raster("F:\\coe1000\\melanie\\indicators\\WIND\\wind_1000.tif")
issue <- raster("F:\\coe1000\\melanie\\indicators\\SOIL_MICRO\\soilmicro_1000.tif")
#issue <- raster("F:\\coe1000\\melanie\\indicators\\POP_CHANGE\\pop_change.tif")


crs(issue) <- st_crs(3035)$wkt 
issue
plot(issue)

### Check Median with SHP
#LC_shp <- st_read("F:\\coe1000\\melanie\\indicators\\LC\\LC_dissolved.shp")
#
#LC_shp$Median <- exact_extract(issue, LC_shp, 'median')
#LC_shp$Median
#LC2 = LC_shp|>st_drop_geometry() ## per togliere la geometria
#[1] 20.34540023  0.96096892  1.48573614  1.10332930  0.02661822  0.08506864


### IF the two input raster are not aligned
##Il file del rusle non era allineato e allora ho applicato il resample
x<-resample(issue,LC, method='ngb')
x
issue <-x


issue <- crop(issue, extent(LC))  
res(issue) <- res(LC) 
extent(issue) <- extent(LC)
       
issue
#crs(LC_shp)

s<- issue
r<- LC
result <- zonal(s, r, 'median')
result
#plot(issue)

#reclassify(r, result, file.path(dirctry_data, "LC_reclassified.tif"))

r2 <- reclassify(r, result)

rc <- function(x) {
  
  x1 <- x[1]  ##valore pixel della prima banda
  x2 <- x[2] ##valore pixel 2 banda
  ifelse(x1 <=  x2, 0,
  ifelse(x1 >  x2, 1, NA)) }


s2 <- stack(s,r2)


beginCluster(6) ##ASSIGN CORE
z1 <- clusterR(s2, calc, args=list(fun=rc), filename=file.path(output_data, "popchange_R.tif"), datatype = "INT1U", overwrite = TRUE)
endCluster()


plot(z1)
z1

#writeRaster(x = z1, paste0(dirctry_data, "/rusleR_R.tif"), format = 'GTiff', overwrite = TRUE)



