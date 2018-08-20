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
GBPU_lut<-readRDS(file = file.path(DataDir,'GBPU_lut'))

# Residential and Commercial Development - Threat 1
# BTM 'Urban'
Residential_1a <-raster(file.path(DataDir,"LandDisturbance/Urban.tif"))
Residential_1b <-raster(file.path(DataDir,"HumanDensityR.tif"))

# Agriculture and Range - Threat 2
# BTM - 'Agriculture', 'Residential Agriculture Mixtures', 'Range Lands'; Stats Can - livestock density
Agriculture_2.1 <- raster(file.path(DataDir,"LandDisturbance/AgricultureR.tif"))
Agriculture_2.3a <- raster(file.path(DataDir,"LandDisturbance/RangeR.tif"))
Agriculture_2.3b<- raster(file.path(DataDir,"LandDisturbance/CowDensityR.tif"))

if (!file.exists(file.path(DataDir,"LandDisturbance/Agriculture_2all.tif"))) {
  Agriculture_2all<- Agriculture_2.1+Agriculture_2.3a
  Agriculture_2all[Agriculture_2all[]>0]<-1
  writeRaster(Agriculture_2all, filename=file.path(DataDir,"LandDisturbance/Agriculture_2all.tif"), format="GTiff", overwrite=TRUE)
} else {
  Agriculture_2all<- raster(file.path(DataDir,"LandDisturbance/Agriculture_2all.tif"))
}

# Energy Production and Mining - Threat 3
# BTM - 'Mining'
#Energy_3.1 <- raster(file.path(DataDir,"LandDisturbance/OilGasR.tif"))
#Energy_3.2 <- raster(file.path(DataDir,"LandDisturbance/Mining.tif"))

Energy_3.1<-
  data.frame(GRIZZLY_BEAR_POP_UNIT_ID=raster::extract(GBPUr, OilGasP)) %>% 
  group_by(GRIZZLY_BEAR_POP_UNIT_ID) %>% 
  dplyr::summarize(count=n())
Energy_3.2<-
  data.frame(GRIZZLY_BEAR_POP_UNIT_ID=raster::extract(GBPUr, MiningP)) %>% 
  group_by(GRIZZLY_BEAR_POP_UNIT_ID) %>% 
  dplyr::summarize(count=n()) 
Energy_3.3<-
  data.frame(GRIZZLY_BEAR_POP_UNIT_ID=raster::extract(GBPUr, WindHydroP)) %>% 
  group_by(GRIZZLY_BEAR_POP_UNIT_ID) %>% 
  dplyr::summarize(count=n())

Energy_3<- merge(Energy_3.1, Energy_3.2, by='GRIZZLY_BEAR_POP_UNIT_ID', all=TRUE) %>%
  merge(Energy_3.3, by='GRIZZLY_BEAR_POP_UNIT_ID', all=TRUE) %>%
  merge(GBPU_lut, by='GRIZZLY_BEAR_POP_UNIT_ID', all.y=TRUE) %>%
  dplyr::select(-POPULATION_NAME)

Energy_3[is.na(Energy_3)] <- 0
colnames(Energy_3)<-c('GRIZZLY_BEAR_POP_UNIT_ID','Energy_3.1','Energy_3.2','Energy_3.3')
Energy_3$Energy_3all<-(Energy_3$Energy_3.1+Energy_3$Energy_3.2+Energy_3$Energy_3.3)
saveRDS(Energy_3, file = (file.path(DataDir,'Energy_3')))

# Transporation & Services Corridors - Threat 4

if (!file.exists(file.path(DataDir,"LandDisturbance/Transport_4.1.tif"))) {
  # Focal function to caluclate a 1km diameter cirular window radius - 564.9769748m
  # multiply result by 0.1 to get km/km2 for each cell
  T1<-Reduce("+",list(RdDensR,RailR))
  T1[T1>0]<-1
  Transport_4.1L<-T1
  writeRaster(Transport_4.1L, filename=file.path(DataDir,"LandDisturbance/Transport_4.1L.tif"), format="GTiff", overwrite=TRUE)
  fw<-focalWeight(raster(res=c(100,100)),565,type='circle')
  fw[fw>0]<-1
  T2 <- focal(T1, w=as.matrix(fw), fun='sum', na.rm=FALSE, pad=TRUE) # to make it km/km2 max should 9.7 km/km2
  #writeRaster(T2, filename=file.path(DataDir,"LandDisturbance/T2.tif"), format="GTiff", overwrite=TRUE)
  #Flag if linear density is > 0.6km/km2, where 1=100m=0.1km
  #Transport_4.1 <- reclassify(T2, c(0,0.6,0,  0.6,200,1))
  #Transport_4.1 <- reclassify(T2, c(0,6,0,  6,200,1))
  Transport_4.1 <- T2
  writeRaster(Transport_4.1, filename=file.path(DataDir,"LandDisturbance/Transport_4.1.tif"), format="GTiff", overwrite=TRUE)
  #Note: function to make a circular matrix for focal function, from:
  #https://scrogster.wordpress.com/2012/10/05/applying-a-circular-moving-window-filter-to-raster-data-in-r/
  #not required using focalWeight instead
} else {
  Transport_4.1L<-raster(file.path(DataDir,"LandDisturbance/Transport_4.1L.tif"))
  Transport_4.1<-raster(file.path(DataDir,"LandDisturbance/Transport_4.1.tif"))
}

if (!file.exists(file.path(DataDir,"LandDisturbance/Transport_4.2.tif"))) {
  # Focal function to caluclate a 1km diameter cirular window radius - 564.9769748m
  # multiply result by 0.1 to get km/km2 for each cell
  #T42<-Reduce("+",list(OilGasR,TransR))
  T42<-Reduce("+",list(OilGasR,TransR,SeismicR))
  T42[T42[]>0]<-1
  Transport_4.2L<-T42
  writeRaster(Transport_4.2L, filename=file.path(DataDir,"LandDisturbance/Transport_4.2L.tif"), format="GTiff", overwrite=TRUE)
  #Note: function to make a circular matrix for focal function, from:
  #https://scrogster.wordpress.com/2012/10/05/applying-a-circular-moving-window-filter-to-raster-data-in-r/
  #not required using focalWeight instead
} else {
  Transport_4.2L<-raster(file.path(DataDir,"LandDisturbance/Transport_4.2L.tif"))
}

#Combined Threat 4
if (!file.exists(file.path(DataDir,"LandDisturbance/Transport_4all.tif"))) {
  # Focal function to caluclate a 1km diameter cirular window radius - 564.9769748m
  # multiply result by 0.1 to get km/km2 for each cell
  Tall1<-Reduce("+",list(RdDensR,RailR,OilGasR,TransR,SeismicR))
  Tall1[Tall1>0]<-1
  Transport_4allL<-Tall1
  writeRaster(Transport_4allL, filename=file.path(DataDir,"LandDisturbance/Transport_4allL.tif"), format="GTiff", overwrite=TRUE)
  fw<-focalWeight(raster(res=c(100,100)),565,type='circle')
  fw[fw>0]<-1
  #T2 <- focal(T1, w=as.matrix(fw[fw>0]<-1), fun='sum', na.rm=FALSE, pad=TRUE)
  Tall2 <- focal(Tall1, w=as.matrix(fw), fun='sum', na.rm=FALSE, pad=TRUE)
  #writeRaster(Tall2, filename=file.path(DataDir,"LandDisturbance/T2all.tif"), format="GTiff", overwrite=TRUE)
  
  #Flag if linear density is > 0.6km/km2
  #Transport_4all <- reclassify(Tall2, c(0,0.6,0,0.6,200,1))
  Transport_4all<-Tall2
  writeRaster(Transport_4all, filename=file.path(DataDir,"LandDisturbance/Transport_4all.tif"), format="GTiff", overwrite=TRUE)
  #Note: function to make a circular matrix for focal function, from:
  #https://scrogster.wordpress.com/2012/10/05/applying-a-circular-moving-window-filter-to-raster-data-in-r/
  #not required using focalWeight instead
} else {
  Transport_4all<-raster(file.path(DataDir,"LandDisturbance/Transport_4all.tif"))
  Transport_4allL<-raster(file.path(DataDir,"LandDisturbance/Transport_4allL.tif"))
}

# Biological Resource Use - Threat 5
# CE Mortality overages
BioUse_5.1a <- raster(file.path(DataDir,"LandDisturbance/Mortr.tif"))
BioUse_5.1b <- raster(file.path(DataDir,"LandDisturbance/HunterDayDr.tif"))
BioUse_5.3 <- raster(file.path(DataDir,"LandDisturbance/MidSeralr.tif"))

# Human Intrusions - Threat 6
# CE Front Country Indicator - cell flagged as front country (1) or not (0)
HumanIntrusion_6 <- raster(file.path(DataDir,"LandDisturbance/FrontCountryr.tif"))

# Salmon Decline - Climate Change - Threat 11
ClimateChange_11 <-raster(file.path(DataDir,"SalmonChangr.tif"))

#Make a Threat brick for assessment
ThreatBrick <- stack(Residential_1a, Residential_1b,Agriculture_2.1,Agriculture_2.3a,Agriculture_2.3b,Agriculture_2all,Transport_4.1,Transport_4.1L,Transport_4.2L,Transport_4all,Transport_4allL,BioUse_5.1a,BioUse_5.1b,BioUse_5.3,HumanIntrusion_6, ClimateChange_11)

names(ThreatBrick) <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Agriculture_2all','Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')

Threat_file <- file.path("tmp/ThreatBrick")
saveRDS(ThreatBrick, file = Threat_file)

#Clean up ranking file
Ranking_full <- data.frame(read.csv(header=TRUE, file=paste(DataDir, "/ProvGBPUs_NatServeMPSimplified.csv", sep=""), sep=",", strip.white=TRUE, ))
saveRDS(Ranking_full, file=file.path(DataDir,'Ranking_full'))
Ranking<-data.frame(GBPU=Ranking_full$GBPU, GBPU_Name=Ranking_full$GBPU_Name,Residential=Ranking_full$Residential,Agriculture=Ranking_full$Agriculture, Energy=Ranking_full$Energy, Transportation=Ranking_full$Transportation, BioUse=Ranking_full$BioUse,HumanIntrusion=Ranking_full$HumanIntrusion,ClimateChange=Ranking_full$ClimateChange)
saveRDS(Ranking, file=file.path(DataDir,'Ranking'))

#Merge Ranking_in isolation with calculated isolation for inspection
Isolation_overal<-
  data.frame(GBPU_Name=Ranking_in$GBPU_Name, Iso_code=Ranking_in$Iso_code) %>%
  merge(Isolation_list[[1]], by.x='GBPU_Name', by.y='GBPU')

Isolation_internal<-
  data.frame(GBPU_Name=Ranking_in$GBPU_Name, Iso_code=Ranking_in$Iso_code) %>%
  merge(Isolation_list[[2]], by.x='GBPU_Name', by.y='GBPU')

Isolation_external<-
  data.frame(GBPU_Name=Ranking_in$GBPU_Name, Iso_code=Ranking_in$Iso_code) %>%
  merge(Isolation_list[[3]], by.x='GBPU_Name', by.y='GBPU')




