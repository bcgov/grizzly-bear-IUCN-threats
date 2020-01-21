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
GBPU_lut<-readRDS(file = file.path(StrataDir,'GBPU_lut'))

# Residential and Commercial Development - Threat 1
# BTM 'Urban'
Residential_1a <-UrbanR
Residential_1b <-HumanDensityR

# Agriculture and Range - Threat 2
# BTM - 'Agriculture', 'Residential Agriculture Mixtures', 'Range Lands'; Stats Can - livestock density
Agriculture_2.1 <- AgricultureR
Agriculture_2.3a <- RangeR
Agriculture_2.3b<- LivestockDensityR

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
saveRDS(Energy_3, file = (file.path(spatialOutDir,'Energy_3')))

# Transporation & Services Corridors - Threat 4

if (!file.exists(file.path(spatialOutDir,"Transport_4.1.tif"))) {
  # Focal function to caluclate a 1km diameter cirular window radius - 564.9769748m
  # multiply result by 0.1 to get km/km2 for each cell
  T1<-Reduce("+",list(RdDensR,RailR))
  T1[T1>0]<-1
  Transport_4.1L<-T1
  writeRaster(Transport_4.1L, filename=file.path(spatialOutDir,"Transport_4.1L.tif"), format="GTiff", overwrite=TRUE)
  fw<-focalWeight(raster(res=c(100,100)),565,type='circle')
  fw[fw>0]<-1
  T2 <- focal(T1, w=as.matrix(fw), fun='sum', na.rm=FALSE, pad=TRUE) # to make it km/km2 max should 9.7 km/km2
  #writeRaster(T2, filename=file.path(spatialOutDir,"T2.tif"), format="GTiff", overwrite=TRUE)
  #Flag if linear density is > 0.6km/km2, where 1=100m=0.1km
  #Transport_4.1 <- reclassify(T2, c(0,0.6,0,  0.6,200,1))
  #Transport_4.1 <- reclassify(T2, c(0,6,0,  6,200,1))
  Transport_4.1 <- T2
  writeRaster(Transport_4.1, filename=file.path(spatialOutDir,"Transport_4.1.tif"), format="GTiff", overwrite=TRUE)
  #Note: function to make a circular matrix for focal function, from:
  #https://scrogster.wordpress.com/2012/10/05/applying-a-circular-moving-window-filter-to-raster-data-in-r/
  #not required using focalWeight instead
} else {
  Transport_4.1L<-raster(file.path(spatialOutDir,"Transport_4.1L.tif"))
  Transport_4.1<-raster(file.path(spatialOutDir,"Transport_4.1.tif"))
}

if (!file.exists(file.path(spatialOutDir,"Transport_4.2.tif"))) {
  # Focal function to caluclate a 1km diameter cirular window radius - 564.9769748m
  # multiply result by 0.1 to get km/km2 for each cell
  #T42<-Reduce("+",list(OilGasR,TransR))
  T42<-Reduce("+",list(OilGasR,TransR,SeismicR))
  T42[T42[]>0]<-1
  Transport_4.2L<-T42
  writeRaster(Transport_4.2L, filename=file.path(spatialOutDir,"Transport_4.2L.tif"), format="GTiff", overwrite=TRUE)
  #Note: function to make a circular matrix for focal function, from:
  #https://scrogster.wordpress.com/2012/10/05/applying-a-circular-moving-window-filter-to-raster-data-in-r/
  #not required using focalWeight instead
} else {
  Transport_4.2L<-raster(file.path(spatialOutDir,"Transport_4.2L.tif"))
}

#Combined Threat 4
if (!file.exists(file.path(spatialOutDir,"Transport_4all.tif"))) {
  # Focal function to caluclate a 1km diameter cirular window radius - 564.9769748m
  # multiply result by 0.1 to get km/km2 for each cell
  Tall1<-Reduce("+",list(RdDensR,RailR,OilGasR,TransR,SeismicR))
  Tall1[Tall1>0]<-1
  Transport_4allL<-Tall1
  writeRaster(Transport_4allL, filename=file.path(spatialOutDir,"Transport_4allL.tif"), format="GTiff", overwrite=TRUE)
  fw<-focalWeight(raster(res=c(100,100)),565,type='circle')
  fw[fw>0]<-1
  #T2 <- focal(T1, w=as.matrix(fw[fw>0]<-1), fun='sum', na.rm=FALSE, pad=TRUE)
  Tall2 <- focal(Tall1, w=as.matrix(fw), fun='sum', na.rm=FALSE, pad=TRUE)
  #writeRaster(Tall2, filename=file.path(spatialOutDir,"T2all.tif"), format="GTiff", overwrite=TRUE)
  
  #Flag if linear density is > 0.6km/km2
  #Transport_4all <- reclassify(Tall2, c(0,0.6,0,0.6,200,1))
  Transport_4all<-Tall2
  writeRaster(Transport_4all, filename=file.path(spatialOutDir,"Transport_4all.tif"), format="GTiff", overwrite=TRUE)
  #Note: function to make a circular matrix for focal function, from:
  #https://scrogster.wordpress.com/2012/10/05/applying-a-circular-moving-window-filter-to-raster-data-in-r/
  #not required using focalWeight instead
} else {
  Transport_4all<-raster(file.path(spatialOutDir,"Transport_4all.tif"))
  Transport_4allL<-raster(file.path(spatialOutDir,"Transport_4allL.tif"))
}

# Biological Resource Use - Threat 5
# CE Mortality overages
BioUse_5.1a <- FemaleUnk_Report_pop %>%
  mutate(GRIZZLY_BEAR_POP_UNIT_ID = GBPU) %>%
  mutate(BioUse_5.1a = pc_Female_Mort_WMU) %>%
  arrange(GRIZZLY_BEAR_POP_UNIT_ID) %>%
  dplyr::select(GRIZZLY_BEAR_POP_UNIT_ID,BioUse_5.1a)

saveRDS(BioUse_5.1a, file = (file.path(dataOutDir,'BioUse_5.1a')))

BioUse_5.1b <- HuntDDensR
BioUse_5.3 <- MidSeralR


# Human Intrusions - Threat 6
# CE Front Country Indicator - cell flagged as front country (1) or not (0)
HumanIntrusion_6 <- FrontCountryR

# Salmon Decline - Climate Change - Threat 11
ClimateChange_11 <-SalmonChangeR

#Make a Threat brick for assessment
ThreatBrick <- stack(Residential_1a, Residential_1b,Agriculture_2.1,Agriculture_2.3a,Agriculture_2.3b,
                     Transport_4.1,Transport_4.1L,Transport_4.2L,Transport_4all,Transport_4allL,
                     BioUse_5.1b,BioUse_5.3,HumanIntrusion_6, ClimateChange_11)

names(ThreatBrick) <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')

Threat_file <- file.path("tmp/ThreatBrick")
saveRDS(ThreatBrick, file = Threat_file)

#Clean up ranking file, assign Adults to 55% of reported population 
ThreatCodeLUT <- data.frame(ThreatCode=c('A','B','C','CD','D'),
                  ExpertOverallThreat=c('VeryHigh','High','Medium','MediumLow','Low'))
ExpertRankLUT <-data.frame(NewRank=c(1,1.5,2,2.5,3,3.5,4,4.5,5),
                           ExpertRank=c('M1','M1M2','M2','M2M3','M3','M3M4','M4','M4M5','M5'))
  
  
Ranking <- 
  Ranking_in %>%
  left_join(GBPop, by='GBPU_Name') %>%
  left_join(Trend, by='GBPU_Name') %>%
  left_join(ThreatCodeLUT, by='ThreatCode') %>%
  left_join(ExpertRankLUT, by='NewRank') %>%
  left_join(Region_LUT, by='GBPU_Name') %>%
  dplyr::select(GBPU_Name, Region, Adults=PopnEst2018,Iso, Trend, ExpertRank, ExpertOverallThreat,
                Residential, Agriculture, Energy, Transportation, BioUse, HumanIntrusion,
                ClimateChange) %>%
  mutate(Adults = round((Adults * 0.55), 0))
                
saveRDS(Ranking, file=file.path(DataDir,'Ranking'))



#Merge Ranking_in isolation with calculated isolation for inspection
Isolation_overal<-
  data.frame(GBPU_Name=Ranking_in$GBPU_Name, Iso_code=Ranking_in$IsoCode) %>%
  merge(Isolation_list[[1]], by.x='GBPU_Name', by.y='GBPU')

Isolation_internal<-
  data.frame(GBPU_Name=Ranking_in$GBPU_Name, Iso_code=Ranking_in$IsoCode) %>%
  merge(Isolation_list[[2]], by.x='GBPU_Name', by.y='GBPU')

Isolation_external<-
  data.frame(GBPU_Name=Ranking_in$GBPU_Name, Iso_code=Ranking_in$IsoCode) %>%
  merge(Isolation_list[[3]], by.x='GBPU_Name', by.y='GBPU')




