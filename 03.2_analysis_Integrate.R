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

#Calculate %indicator area of total strata area for certain indicators
IndsPCArea<-c("Area","Residential_1a","Agriculture_2.1","Agriculture_2.3a","Agriculture_2all","BioUse_5.1a","BioUse_5.3","HumanIntrusion_6")
IndsPCLength<-c("Area","Transport_4.1L","Transport_4.2L","Transport_4allL")
IndsCount<-c("Energy_3","Energy_3.1","Energy_3.2","Energy_3.3")
IndsDensity<-c("Area","Residential_1b","Agriculture_2.3b","BioUse_5.1b","ClimateChange_11")
KmDensity<-c("Area","Transport_4.1","Transport_4all")

Strata<-c('GRIZZLY_BEAR_POP_UNIT_ID','POPULATION_NAME')
StrataL <- c('GBPUr','GBPUr_NonHab','GBPUr_BEI_1_2','GBPUr_BEI_1_5','GBPUr_LFormFlat','GBPUr_Forest')
num<-length(StrataL)
ThreatL<-list()

# Loop through each Strata concatenate and generate a Strata list of data frames
for (i in 1:num) {
  StratName<-StrataL[i]
  ThreatZone <- data.frame(readRDS(file = (file.path(StrataOutDir,StratName))))
  
  # Area indicators expressed as % indicator of strata 
  GBlistA<-ThreatZone[ , (names(ThreatZone) %in% IndsPCArea)]
  GBA<-data.frame(lapply(GBlistA, function(x) round(x*100/GBlistA$Area,4)))
  # rank area indicators
  GBAR<-data.frame(lapply(GBA, function(x) rank(x, ties.method='random')))
  colnames(GBAR)<-lapply(IndsPCArea, function(x) paste(x, 'Rank',sep=''))
  
  # Linear indicators expressed as km/area of Strata
  GBlistL<-ThreatZone[ , (names(ThreatZone) %in% IndsPCLength)]
  GBL<-data.frame(lapply(GBlistL, function(x) round(x/(10*GBlistL$Area),4))) #0.1km of util lines, 0.1km/km2 = 1km/(10*km2)
  # rank linear indicators
  GBLR<-data.frame(lapply(GBlistL, function(x) rank(x, ties.method='random')))
  colnames(GBLR)<-lapply(IndsPCLength, function(x) paste(x, 'Rank',sep=''))
  
  # Density indicators expressed as #/ha in Strata
  GBlistD<-ThreatZone[ , (names(ThreatZone) %in% IndsDensity)]
  GBD<-data.frame(lapply(GBlistD, function(x) round((x)/GBlistD$Area,4)))#density summed to area of zone/area of unit to get density per/km2
  # rank density indicators
  GBDR<-data.frame(lapply(GBlistD, function(x) rank(x, ties.method='random')))
  colnames(GBDR)<-lapply(IndsDensity, function(x) paste(x, 'Rank',sep=''))
  
  # Km Density indicators expressed as km/km2 in Strata
  GBlistDK<-ThreatZone[ , (names(ThreatZone) %in% KmDensity)]
  GBDK<-data.frame(lapply(GBlistDK, function(x) round((x)/GBlistDK$Area/10,4)))#metres of rds/ha (metre/ha)/10 to translate to km/km2
  # rank density indicators
  GBDKR<-data.frame(lapply(GBlistDK, function(x) rank(x, ties.method='random')))
  colnames(GBDKR)<-lapply(KmDensity, function(x) paste(x, 'Rank',sep=''))
  
  # Energy_3 indicators expressed as number in Strata
  GBC<-readRDS(file = (file.path(DataDir,'Energy_3')))
  
  ThreatZ<-
    cbind(data.frame(ThreatZone[ , (names(ThreatZone) %in% Strata)], GBA, GBAR, GBL, GBLR, GBC, GBD, GBDR, GBDK, GBDKR)) %>%
    dplyr::select(-GRIZZLY_BEAR_POP_UNIT_ID.1,-Area.1,-Area.2, -Area.3)
  
  #Save individual Strata
  ThreatZ_file <- file.path(dataOutDir,paste("ThreatZ_",StrataL[i], sep=""))
  saveRDS(ThreatZ, file = ThreatZ_file)
  
  ThreatL[[StratName]]<-ThreatZ
}

#Merge in the Ranking info into each strata file
Mergefunc <- function(x,y){merge(x, y, by.x=names(x)[2], by.y=names(y)[2])}
Ranking<-readRDS(file=file.path(DataDir,'Ranking'))

ThreatLR<-lapply(ThreatL, Mergefunc, Ranking)

#For each strata pull out the relevant attributes and build an ordered data frame for inspection
ThreatLZR<-list()

for (i in 1:num) {
  StratName<-StrataL[i]
  ThreatLZR[[StratName]]<-data.frame(GBPU_Name=ThreatLR[[StrataL[i]]]$POPULATION_NAME,
                                     Residential_1a=ThreatLR[[StrataL[i]]]$Residential_1a,
                                     Residential_1aRank=ThreatLR[[StrataL[i]]]$Residential_1aRank,
                                     Residential_1b=ThreatLR[[StrataL[i]]]$Residential_1b,
                                     Residential_1bRank=ThreatLR[[StrataL[i]]]$Residential_1bRank,
                                     Residential=ThreatLR[[StrataL[i]]]$Residential,
                                     Agriculture_2.1=ThreatLR[[StrataL[i]]]$Agriculture_2.1,
                                     Agriculture_2.1Rank=ThreatLR[[StrataL[i]]]$Agriculture_2.1Rank,
                                     Agriculture_2.3a=ThreatLR[[StrataL[i]]]$Agriculture_2.3a,
                                     Agriculture_2.3aRank=ThreatLR[[StrataL[i]]]$Agriculture_2.3aRank,
                                     Agriculture_2.3b=ThreatLR[[StrataL[i]]]$Agriculture_2.3b,
                                     Agriculture_2.3bRank=ThreatLR[[StrataL[i]]]$Agriculture_2.3bRank,
                                     Agriculture_2all=ThreatLR[[StrataL[i]]]$Agriculture_2all,
                                     Agriculture_2allRank=ThreatLR[[StrataL[i]]]$Agriculture_2allRank,
                                     Agriculture=ThreatLR[[StrataL[i]]]$Agriculture,
                                     Energy_3.1=ThreatLR[[StrataL[i]]]$Energy_3.1,
                                     Energy_3.2=ThreatLR[[StrataL[i]]]$Energy_3.2,
                                     Energy_3.3=ThreatLR[[StrataL[i]]]$Energy_3.3,
                                     Energy_3all=ThreatLR[[StrataL[i]]]$Energy_3all,
                                     Energy=ThreatLR[[StrataL[i]]]$Energy,
                                     Transport_4.1=ThreatLR[[StrataL[i]]]$Transport_4.1,
                                     Transport_4.1L=ThreatLR[[StrataL[i]]]$Transport_4.1L,
                                     Transport_4.1Rank=ThreatLR[[StrataL[i]]]$Transport_4.1Rank,
                                     Transport_4.2L=ThreatLR[[StrataL[i]]]$Transport_4.2L,
                                     Transport_4.2Rank=ThreatLR[[StrataL[i]]]$Transport_4.2LRank,
                                     Transport_4all=ThreatLR[[StrataL[i]]]$Transport_4all,
                                     Transport_4allL=ThreatLR[[StrataL[i]]]$Transport_4allL,
                                     Transport_4allRank=ThreatLR[[StrataL[i]]]$Transport_4allRank,
                                     Transportation=ThreatLR[[StrataL[i]]]$Transportation,
                                     BioUse_5.1a=ThreatLR[[StrataL[i]]]$BioUse_5.1a,
                                     BioUse_5.1aRank=ThreatLR[[StrataL[i]]]$BioUse_5.1aRank,
                                     BioUse_5.1b=ThreatLR[[StrataL[i]]]$BioUse_5.1b,
                                     BioUse_5.1Rank=ThreatLR[[StrataL[i]]]$BioUse_5.1bRank,
                                     BioUse_5.3=ThreatLR[[StrataL[i]]]$BioUse_5.3,
                                     BioUse_5.3Rank=ThreatLR[[StrataL[i]]]$BioUse_5.3Rank,
                                     BioUse=ThreatLR[[StrataL[i]]]$BioUse,
                                     HumanIntrusion_6=ThreatLR[[StrataL[i]]]$HumanIntrusion_6,
                                     HumanIntrusion_6Rank=ThreatLR[[StrataL[i]]]$HumanIntrusion_6Rank,
                                     HumanIntrusion=ThreatLR[[StrataL[i]]]$HumanIntrusion,
                                     ClimateChange_11=ThreatLR[[StrataL[i]]]$ClimateChange_11,
                                     ClimateChange_11Rank=ThreatLR[[StrataL[i]]]$ClimateChange_11Rank,
                                     ClimateChange=ThreatLR[[StrataL[i]]]$ClimateChange
  )
  #write_csv(ThreatLZR[[StratName]], file.path(dataOutDir,paste(StratName,'.csv',sep='')))
}
# write out the list of threat strata data frames to a multi-tab excel spreadsheet
WriteXLS(ThreatLZR, file.path(dataOutDir,paste('GBThreats.xls',sep='')),SheetNames=StrataL)

#Make a single data.frame using the most appropriate strata and write out for further evaluation
ThreatI<-data.frame(GBPU_Name=ThreatLR[['GBPUr']]$POPULATION_NAME,
                    Residential_1a=ThreatLR[['GBPUr_LFormFlat']]$Residential_1a,
                    Residential_1aRank=ThreatLR[['GBPUr_LFormFlat']]$Residential_1aRank,
                    Residential_1b=ThreatLR[['GBPUr_LFormFlat']]$Residential_1b,
                    Residential_1bRank=ThreatLR[['GBPUr_LFormFlat']]$Residential_1bRank,
                    Residential=ThreatLR[['GBPUr']]$Residential,
                    Agriculture_2.1=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2.1,
                    Agriculture_2.1Rank=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2.1Rank,
                    Agriculture_2.3a=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2.3a,
                    Agriculture_2.3aRank=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2.3aRank,
                    Agriculture_2.3b=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2.3b,
                    Agriculture_2.3bRank=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2.3bRank,
                    Agriculture_2all=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2all,
                    Agriculture_2allRank=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2allRank,
                    Agriculture=ThreatLR[['GBPUr']]$Agriculture,
                    Energy_3.1=ThreatLR[['GBPUr']]$Energy_3.1,
                    Energy_3.2=ThreatLR[['GBPUr']]$Energy_3.2,
                    Energy_3.3=ThreatLR[['GBPUr']]$Energy_3.3,
                    Energy_3all=ThreatLR[['GBPUr']]$Energy_3all,
                    Energy=ThreatLR[['GBPUr']]$Energy,
                    Transport_4.1=ThreatLR[['GBPUr_NonHab']]$Transport_4.1,
                    Transport_4.1L=ThreatLR[['GBPUr_NonHab']]$Transport_4.1L,
                    Transport_4.1Rank=ThreatLR[['GBPUr_NonHab']]$Transport_4.1Rank,
                    Transport_4.2L=ThreatLR[['GBPUr_NonHab']]$Transport_4.2L,
                    Transport_4.2LRank=ThreatLR[['GBPUr_NonHab']]$Transport_4.2LRank,
                    Transport_4all=ThreatLR[['GBPUr_NonHab']]$Transport_4all,
                    Transport_4allL=ThreatLR[['GBPUr_NonHab']]$Transport_4allL,
                    Transport_4allRank=ThreatLR[['GBPUr_NonHab']]$Transport_4allRank,
                    Transportation=ThreatLR[['GBPUr']]$Transportation,
                    BioUse_5.1a=ThreatLR[['GBPUr_NonHab']]$BioUse_5.1a,
                    BioUse_5.1aRank=ThreatLR[['GBPUr_NonHab']]$BioUse_5.1aRank,
                    BioUse_5.1b=ThreatLR[['GBPUr_NonHab']]$BioUse_5.1b,
                    BioUse_5.1bRank=ThreatLR[['GBPUr_NonHab']]$BioUse_5.1bRank,
                    BioUse_5.3=ThreatLR[['GBPUr_Forest']]$BioUse_5.3,
                    BioUse_5.3Rank=ThreatLR[['GBPUr_Forest']]$BioUse_5.3Rank,
                    BioUse=ThreatLR[['GBPUr']]$BioUse,
                    HumanIntrusion_6=ThreatLR[['GBPUr_NonHab']]$HumanIntrusion_6,
                    HumanIntrusion_6Rank=ThreatLR[['GBPUr_NonHab']]$HumanIntrusion_6Rank,
                    HumanIntrusion=ThreatLR[['GBPUr']]$HumanIntrusion,
                    ClimateChange_11=ThreatLR[['GBPUr_NonHab']]$ClimateChange_11,
                    ClimateChange_11Rank=ThreatLR[['GBPUr_NonHab']]$ClimateChange_11Rank,
                    ClimateChange=ThreatLR[['GBPUr']]$ClimateChange)

WriteXLS(ThreatI, file.path(dataOutDir,paste('GBThreatsI.xls',sep='')))
