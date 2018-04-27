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
IndsPCArea<-c("Area","Residential_1","Agriculture_2.1","Agriculture_2.3a","Agriculture_2.3b","Energy_3.2","Transport_4.1","BioUse_5.1","BioUse_5.3","HumanIntusion_6")
IndsPCLength<-c("Area","Transport_4.2")
Strata<-c('GRIZZLY_BEAR_POP_UNIT_ID','POPULATION_NAME')
StrataL <- c('GBPUr','GBPUr_NonHab','GBPUr_BEI_1_2','GBPUr_BEI_1_5','GBPUr_LFormFlat','GBPUr_LFormFlatFlat')
num<-length(StrataL)
ThreatL<-list()

# Loop through each Strata concatenate and generate a Strata list of data frames
for (i in 1:num) {
  StratName<-StrataL[i]
  ThreatZone <- data.frame(readRDS(file = (file.path(StrataOutDir,StratName))))

  # Area indicators expressed as %of ha of indicator/ha of strata
  GBlistA<-ThreatZone[ , (names(ThreatZone) %in% IndsPCArea)]
  GBA<-data.frame(lapply(GBlistA, function(x) round(x/GBlistA$Area*100,2)))
  # Linear indicators expressed as km/area of Strata
  GBlistL<-ThreatZone[ , (names(ThreatZone) %in% IndsPCLength)]
  GBL<-data.frame(lapply(GBlistL, function(x) round((x*100)/GBlistL$Area,4)))
  ThreatZ<-cbind(data.frame(ThreatZone[ , (names(ThreatZone) %in% Strata)], GBA, GBL))
  
  #Save individual Strata
  ThreatZ_file <- file.path(dataOutDir,paste("ThreatZ_",StrataL[i], sep=""))
  saveRDS(ThreatZ, file = ThreatZ_file)
  
  ThreatL[[StratName]]<-ThreatZ
}

#Merge in the Ranking info into each strata file
Mergefunc <- function(x,y){merge(x, y, by.x=names(x)[2], by.y=names(y)[2])}
ThreatLR<-lapply(ThreatL, Mergefunc, Ranking)

#For each strata pull out the relevant attributes and build an ordered data frame for inspection
ThreatLZR<-list()

for (i in 1:num) {
  StratName<-StrataL[i]
  ThreatLZR[[StratName]]<-data.frame(GBPU_Name=ThreatLR[[StrataL[i]]]$POPULATION_NAME,
                  Residential_1=ThreatLR[[StrataL[i]]]$Residential_1,
                  Residential=ThreatLR[[StrataL[i]]]$Residential,
                  Agriculture_2=ThreatLR[[StrataL[i]]]$Agriculture_2.1,
                  Agriculture_2=ThreatLR[[StrataL[i]]]$Agriculture_2.3a,
                  Agriculture_2=ThreatLR[[StrataL[i]]]$Agriculture_2.3b,
                  Agriculture=ThreatLR[[StrataL[i]]]$Agriculture,
                  Energy_3.2=ThreatLR[[StrataL[i]]]$Energy_3.2,
                  Energy=ThreatLR[[StrataL[i]]]$Energy,
                  Transport_4.1=ThreatLR[[StrataL[i]]]$Transport_4.1,
                  Transport_4.2=ThreatLR[[StrataL[i]]]$Transport_4.2,
                  Transportation=ThreatLR[[StrataL[i]]]$Transportation,
                  BioUse_5.1=ThreatLR[[StrataL[i]]]$BioUse_5.1,
                  BioUse_5.3=ThreatLR[[StrataL[i]]]$BioUse_5.3,
                  BioUse=ThreatLR[[StrataL[i]]]$BioUse,
                  HumanIntusion_6=ThreatLR[[StrataL[i]]]$HumanIntusion_6,
                  HumanIntusion=ThreatLR[[StrataL[i]]]$HumanIntusion
                  )
  write_csv(ThreatLZR[[StratName]], file.path(dataOutDir,paste(StratName,'.csv',sep='')))
}

#Make a single data.frame using the most appropriate strata and write out for further evaluation
