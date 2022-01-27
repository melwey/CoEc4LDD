library(terra)
library(raster)
library(sf)
library(exactextractr)
library(rgdal)


dirctry_data <- "F:/coe1000/melanie/indicators"

LC <- raster("F:\\coe1000\\melanie\\indicators\\LC\\LC.tif")
#crs(LC) <- sp::CRS("EPSG:3035")
#sp::CRS("EPSG:3035")
#crs(LC)<-CRS("+init=epsg:3035")


builtup <- raster("F:\\coe1000\\melanie\\indicators\\RUSLE\\rusle2.tif")

LC_shp <- st_read("F:\\coe1000\\melanie\\indicators\\LC\\LC_dissolved.shp")

LC_shp$Median <- exact_extract(builtup, LC_shp, 'median')
LC_shp$Median
LC2 = LC_shp|>st_drop_geometry() ## per togliere la geometria

       

#crs(LC_shp)

s<- builtup
r<- LC
result <- zonal(s, r, 'median')
result

## Result

#zone      median
#[1,]    0  0.02380885
#[2,]    1  1.53679210
#[3,]    2  0.97458199
#[4,]    3  0.08090679
#[5,]    4  1.17528319
#[6,]    5  0.03201752
#[7,]    6 20.96264839

#r2= r
#r2[r==0]<-0.0
#r2[r==1]<-1.53679210
#r2[r==2]<-0.97458199
#r2[r==3]<-0.08090679
#r2[r==4]<-1.17528319
#r2[r==5]<-0.03201752
#r2[r==6]<-20.96264839
#
#plot(r2)

reclassify(r, result, file.path(dirctry_data, "LC_reclassified2.tif"))


rc <- function(x) {
  
  x1 <- x[1]  ##valore pixel della prima banda
  x2 <- x[2] ##valore pixel 2 banda
  ifelse(x1 <=  x2, 0,
  ifelse(x1 >  x2, 1, NA)) }


s2 <- stack(s,r)


beginCluster(4) ##ASSIGN CORE
z1 <- clusterR(s2, calc, args=list(fun=rc), filename=file.path(dirctry_data, "rusle2R_R.tif"), datatype = "INT1U", overwrite = TRUE)
endCluster()

plot(z1)
z1

#writeRaster(x = z1, paste0(dirctry_data, "/rusleR_R.tif"), format = 'GTiff', overwrite = TRUE)
