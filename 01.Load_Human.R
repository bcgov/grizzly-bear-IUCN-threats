# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#people/km2 by  Stats Can - by Dissemination Block (DB)
#A dissemination block (DB) is an area bounded on all sides by roads and/or boundaries of standard geographic areas. The dissemination block is the smallest geographic area for which population and dwelling counts are disseminated. Dissemination blocks cover all the territory of Canada.
#
#
#

source("header.R")

ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                 ymn=173787.5, ymx=1748187.5, 
                 crs="+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",
                 res = c(100,100), vals = 0)

BCr_file <- file.path(dataOutDir,"BCr.tif")
if (!file.exists(BCr_file)) {
  BCr <- fasterize(bcmaps::bc_bound_hres(class='sf'),ProvRast)
  writeRaster(BCr, filename=BCr_file, format="GTiff", overwrite=TRUE)
} else {
  BCr <- raster(BCr_file)
}

#Read in  'Census consolidated subdivisions' finest resolution for cows and sheep data
#Geographic -https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm
#Projection is from https://www150.statcan.gc.ca/n1/pub/92-160-g/92-160-g2016002-eng.htm
CensusDisBlk_LCCproj<-st_read(dsn=file.path(DataDir,'HumanDensity'), stringsAsFactors = FALSE,
        crs = '+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

#Transform from Stats Canada Lambert: +proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#To BC Lambert:  +proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
CensusDisBlk = st_transform(CensusDisBlk_LCCproj, "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 

#Read in sheep and cows and subset to Total amounts
#Data - https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/comprehensive.cfm
Humans <- data.frame(read.csv(header=TRUE, file=file.path(DataDir,'HumanDensity/T1901EN.CSV'), strip.white=TRUE, ))
#TotalHumans<-subset.data.frame(Humans, Sheep.and.lambs=='Total sheep and lambs' & Unit.of.measure=='Number of animals' & REF_DATE==2016)

#Create a combined data.frame with total livestock, set NA to 0 and 
#comparable index based on DGUID to join the Censuse spatial
Human_DB<-
  Humans %>%
  dplyr::select(one_of(c('Geographic.code','Population..2016','Land.area.in.square.kilometres..2016','Population.density.per.square.kilometre..2016')))

colnames(Human_DB)<-c('DAUID','TotalHumans','AreaOnFile_KM2','HumanDensity')

#Link to the spatial and fasterize
HumanMap<-merge(CensusDisBlk, Human_DB, by='DAUID')
#Report out humans/km2
HumanMap$AreaHa = as.single(st_area(HumanMap)/10000)
HumanMap$Hdensity_KM2=HumanMap$TotalHumans/HumanMap$AreaHa*100
#Dump as shape to inspect.
st_write(HumanMap, file.path(DataDir,'HumanDensity/HumanDensity.shp'), delete_layer = TRUE)
                              
HumanDensityR<-mask(fasterize(HumanMap, ProvRast, field='HumanDensity'),BCr)
writeRaster(HumanDensityR, filename=file.path(DataDir,"HumanDensityR.tif"), format="GTiff", overwrite=TRUE)

