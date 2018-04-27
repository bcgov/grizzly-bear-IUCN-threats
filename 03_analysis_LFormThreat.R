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

StrataL <- c('GBPUr','GBPUr_NonHab','GBPUr_BEI_1_2','GBPUr_BEI_1_5','GBPUr_LFormFlat')
GBPU_lut<-readRDS(file = 'tmp/GBPU_lut')

# Load LForm Data
LForm <- raster(file.path(DataDir,"LForm.tif"))

# Load all strata to compare
NonHab<-raster(file.path(DataDir,"NonHab.tif"))
GBPUr<-raster(file.path(DataDir,"Strata/GBPUr.tif"))
GBPUr_NonHab<-raster(file.path(DataDir,"Strata/GBPUr_NonHab.tif"))
GBPUr_BEI_1_2<-raster(file.path(DataDir,"Strata/GBPUr_BEI_1_2.tif"))
GBPUr_BEI_1_5<-raster(file.path(DataDir,"Strata/GBPUr_BEI_1_5.tif"))
GBPUr_LFormFlat<-raster(file.path(DataDir,"Strata/GBPUr_LFormFlat.tif"))

# Set GBStrata to LForm
  LForm <- raster(file.path(DataDir,"LForm.tif"))
  LF_lut<-read_csv(file.path(DataDir,'landform_lut.csv'), col_names=TRUE)
  
  ThreatZoneF<-freq(LForm, parellel=FALSE)
  
  colnames(ThreatZoneF)<-c('ID','Area')
  Landforms<-merge(LF_lut,ThreatZoneF,by='ID')
  
  ThreatZ1<-zonal(ThreatBrick,LForm,fun='sum',parallel=FALSE)
  
  LFormXThreat<-merge(Landforms,ThreatZ1,by.x='ID',by.y='zone')
  
  write_csv(LFormXThreat, file.path(dataOutDir,('LFormXThreat.csv')))
