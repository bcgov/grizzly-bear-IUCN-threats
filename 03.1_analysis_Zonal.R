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

## Copy and run in R outside of R studio - R studio has a memory allocation bug running zonal and freq
DataDir <- 'data'
StrataOutDir<-'out/data/Strata'
library(raster)
setwd('/Users/Morgan/Dropbox (BVRC)/_dev/grizzly-bear-IUCN-threats')

Threat_file <- file.path("tmp/ThreatBrick")
ThreatBrick <- readRDS(file = Threat_file)

StrataL <- c('GBPUr','GBPUr_NonHab','GBPUr_BEI_1_2','GBPUr_BEI_1_5','GBPUr_LFormFlat','GBPUr_Forest')
GBPU_lut<-readRDS(file = 'tmp/GBPU_lut')

num<-length(StrataL)
i<-1
for (i in 1:num) {
  # Originally strata was in a brick, but freq and zonal had memory issues
  GBStrata<-raster(file.path(DataDir,'Strata',paste(StrataL[i], ".tif", sep="")))
  
  ThreatZoneF<-freq(GBStrata, parellel=FALSE)
  colnames(ThreatZoneF)<-c('GRIZZLY_BEAR_POP_UNIT_ID','Area')
  ThreatGBPU<-merge(GBPU_lut,ThreatZoneF,by='GRIZZLY_BEAR_POP_UNIT_ID')
  
  ThreatZ1<-zonal(ThreatBrick,GBStrata,'sum', na.rm=TRUE)
  
  ThreatZone<-merge(ThreatGBPU,ThreatZ1,by.x='GRIZZLY_BEAR_POP_UNIT_ID',by.y='zone')
  
  saveRDS(ThreatZone, file = (file.path(StrataOutDir,StrataL[i])))
}

