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

source("header.R")

#Identify Strata layers - read from disk for analysis
StrataL <- c('GBPUr','GBPUr_NonHab','GBPUr_BEI_1_2','GBPUr_BEI_1_5','GBPUr_LFormFlat')

# Residential and Commercial Development - Threat 1
# BTM 'Urban'
Residential_1 <-raster(file.path(DataDir,"LandDisturbance/Urban.tif"))

# Agriculture and Range - Threat 2
# BTM - 'Agriculture', 'Residential Agriculture Mixtures', 'Range Lands'; Stats Can - livestock density
Agriculture_2.1 <- raster(file.path(DataDir,"LandDisturbance/AgricultureR.tif"))
Agriculture_2.3a <- raster(file.path(DataDir,"LandDisturbance/RangeR.tif"))
Agriculture_2.3b<- raster(file.path(DataDir,"LandDisturbance/CowDensityR.tif"))

# Energy Production and Mining - Threat 3
# BTM - 'Mining'
#Energy_3.1 <- raster(file.path(DataDir,"LandDisturbance/OilGasR.tif"))
Energy_3.2 <- raster(file.path(DataDir,"LandDisturbance/Mining.tif"))

# Transporation & Services Corridors - Threat 4

if (!file.exists(file.path(DataDir,"LandDisturbance/Transport_4.1.tif"))) {
  # Focal function to caluclate a 1km diameter cirular window radius - 564.9769748m
  # multiply result by 0.1 to get km/km2 for each cell
  T1<-Reduce("+",list(RdDensR,RailR))
  fw<-focalWeight(raster(res=c(100,100)),565,type='circle')
  T2 <- focal(T1, w=as.matrix(fw[fw >0]<-1), fun='sum', na.rm=FALSE, pad=TRUE)
  #Flag if linear density is > 0.6km/km2
  Transport_4.1 <- reclassify(T2, c(0,6,0,  6,200,1))
  writeRaster(Transport_4.1, filename=file.path(DataDir,"LandDisturbance/Transport_4.1.tif"), format="GTiff", overwrite=TRUE)
  #Note: function to make a circular matrix for focal function, from:
  #https://scrogster.wordpress.com/2012/10/05/applying-a-circular-moving-window-filter-to-raster-data-in-r/
  #not required using focalWeight instead
} else {
  Transport_4.1<-raster(file.path(DataDir,"LandDisturbance/Transport_4.1.tif"))
}

if (!file.exists(file.path(DataDir,"LandDisturbance/Transport_4.2.tif"))) {
  # Focal function to caluclate a 1km diameter cirular window radius - 564.9769748m
  # multiply result by 0.1 to get km/km2 for each cell
  T42<-Reduce("+",list(OilGasR,TransR))
  T42[T42[]>0]<-1
  Transport_4.2<-T42
  writeRaster(Transport_4.2, filename=file.path(DataDir,"LandDisturbance/Transport_4.2.tif"), format="GTiff", overwrite=TRUE)
  #Note: function to make a circular matrix for focal function, from:
  #https://scrogster.wordpress.com/2012/10/05/applying-a-circular-moving-window-filter-to-raster-data-in-r/
  #not required using focalWeight instead
} else {
  Transport_4.2<-raster(file.path(DataDir,"LandDisturbance/Transport_4.2.tif"))
}

# Biological Resource Use - Threat 5
# CE Mortality overages
BioUse_5.1 <- raster(file.path(DataDir,"LandDisturbance/Mortr.tif"))
BioUse_5.3 <- raster(file.path(DataDir,"LandDisturbance/MidSeralr.tif"))

# Human Intrusions - Threat 6
# CE Front Country Indicator
HumanIntusion_6 <- raster(file.path(DataDir,"LandDisturbance/FrontCountryr.tif"))

#Make a Threat brick for assessment
ThreatBrick <- stack(Residential_1,Agriculture_2.1,Agriculture_2.3a,Agriculture_2.3b,Energy_3.1,Energy_3.2,Transport_4.1,BioUse_5.1,BioUse_5.3,HumanIntusion_6)

names(ThreatBrick) <- c('Residential_1','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Energy_3.2','Transport_4.1','Transport_4.2','BioUse_5.1','BioUse_5.3','HumanIntusion_6')

Threat_file <- file.path("tmp/ThreatBrick")
saveRDS(ThreatBrick, file = Threat_file)

#Clean up ranking file
Ranking<-data.frame(GBPU=Ranking_in$GBPU, GBPU_Name=Ranking_in$GBPU_Name,Residential=Ranking_in$Residential,Agriculture=Ranking_in$Agriculture, Energy=Ranking_in$Energy, Transportation=Ranking_in$Transportation, BioUse=Ranking_in$BioUse,HumanIntusion=Ranking_in$HumanIntusion)





