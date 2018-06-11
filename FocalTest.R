
## Copy and run in R outside of R studio
DataDir <- 'data'
library(raster)
setwd('/Users/Morgan/Dropbox (BVRC)/_dev/grizzly-bear-IUCN-threats')

Threat_file <- file.path("tmp/ThreatBrick")
ThreatBrick <- readRDS(file = Threat_file)
#VT4.1<-zonal(Railr,GBPUr,fun='sum')
ThreatZone<-zonal(ThreatBrick,GBPUr,fun='sum')
saveRDS(Threat, file = "tmp/ThreatZone")
###

ThreatZone <- readRDS(file = "tmp/ThreatZone")




#########
#Railr<-raster(file.path(DataDir,"LandDisturbance/RailwayP.tif"))*100
#GBPU<-readRDS(file.path("tmp/GBPU"))
#GBPUr<-fasterize(GBPU, ProvRast)
#writeRaster(GBPUr, filename=file.path(DataDir,"LandDisturbance/GBPUr.tif"), format="GTiff", overwrite=TRUE)

