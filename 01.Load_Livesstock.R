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

#livestock/km2 by Stats Can -  by Census Consolidated Subdivision  (CCS) 
#A census consolidated subdivision (CCS) is a group of adjacent census subdivisions within the same census division. Generally, the smaller, more densely-populated census subdivisions (towns, villages, etc.) are combined with the surrounding, larger, more rural census subdivision, in order to create a geographic level between the census subdivision and the census division.
#spatial file of Census consolidated subdivisions https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm-
#


source("header.R")
library(stringr)

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
#Map available from: https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm
#Projection is from https://www150.statcan.gc.ca/n1/pub/92-160-g/92-160-g2016002-eng.htm
#file name lccs000a16a_e
#
CensusConSubs_LCCproj<-st_read(dsn=file.path(DataDir,'Livestock'), stringsAsFactors = FALSE,
        crs = '+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

#Transform from Stats Canada Lambert: +proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#To BC Lambert:  +proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
CensusConSubs = st_transform(CensusConSubs_LCCproj, "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 

#Read in sheep and cows and subset to Total amounts
Sheep <- data.frame(read.csv(header=TRUE, file=file.path(DataDir,'Livestock/32100425-eng/32100425.csv'), strip.white=TRUE, ))
TotalSheep<-subset.data.frame(Sheep, Sheep.and.lambs=='Total sheep and lambs' & Unit.of.measure=='Number of animals' & REF_DATE==2016)

Cows <- data.frame(read.csv(header=TRUE, file=file.path(DataDir,'Livestock/32100424-eng/32100424.csv'), strip.white=TRUE, ))
TotalCows<-subset.data.frame(Cows, Cattle.and.calves=='Total cows' & Unit.of.measure=='Number of animals' & REF_DATE==2016)

#Create a combined data.frame with total livestock, set NA to 0 and 
#comparable index based on DGUID to join the Censuse spatial
Livestock_DB<-
  TotalCows %>%
  left_join(TotalSheep, by='DGUID') %>%
  mutate(NCows = VALUE.x) %>%
  mutate(NSheep = VALUE.y) %>%
  mutate(GEO = GEO.x) %>%
  mutate(TotalLivestock = NCows + NSheep) %>%
  replace(., is.na(.), 0) %>%
  mutate(ID = str_sub(DGUID, start=-7)) %>%
  dplyr::select(one_of(c('ID', 'GEO', 'TotalLivestock', 'NCows','NSheep')))

#Link to the spatial
LSMap<-merge(CensusConSubs, Livestock_DB, by.x='CCSUID', by.y='ID')
  LSMap$AreaHa = as.single(st_area(LSMap)/10000)
  LSMap$LSdensity_km2=LSMap$TotalLivestock/LSMap$AreaHa*100

#Dump as shape to inspect.
st_write(LSMap, file.path(DataDir,'HumanDensity/HumanDensity.shp'), delete_layer = TRUE)

#fasterize and write to directory for 02_clean.R script
LiveStockDensityR<-mask(fasterize(LSMap, ProvRast, field='LSdensity_km2'),BCr)
#CowDensityR<-setValues(raster(CDR1), CDR1[]) #vestigual code

writeRaster(LiveStockDensityR, filename=file.path(DataDir,"LandDisturbance/LSDensityR.tif"), format="GTiff", overwrite=TRUE)

