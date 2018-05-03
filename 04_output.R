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
IndsPCArea<-c("Area","Residential_1","Agriculture_2.1","Agriculture_2.3a","Transport_4.1","BioUse_5.1a","BioUse_5.3","HumanIntrusion_6")
IndsPCLength<-c("Area","Transport_4.2")
IndsCount<-c("Energy_3.1","Energy_3.2","Energy_3.3")
IndsDensity<-c("Area","Agriculture_2.3b","BioUse_5.1b")

Strata<-c('GRIZZLY_BEAR_POP_UNIT_ID','POPULATION_NAME')
StrataL <- c('GBPUr','GBPUr_NonHab','GBPUr_BEI_1_2','GBPUr_BEI_1_5','GBPUr_LFormFlat','GBPUr_Forest')
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
  # Density indicators expressed as #/ha in Strata
  GBlistD<-ThreatZone[ , (names(ThreatZone) %in% IndsDensity)]
  GBD<-data.frame(lapply(GBlistD, function(x) round((x)/GBlistD$Area,2)))
  # Energy_3 indicators expressed as number in Strata
  GBC<-readRDS(file = (file.path(DataDir,'Energy_3')))
  
  ThreatZ<-
    cbind(data.frame(ThreatZone[ , (names(ThreatZone) %in% Strata)], GBA, GBL, GBC, GBD)) %>%
    dplyr::select(-GRIZZLY_BEAR_POP_UNIT_ID.1,-Area.1,-Area.2)
  
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
                                     Agriculture_2.1=ThreatLR[[StrataL[i]]]$Agriculture_2.1,
                                     Agriculture_2.3a=ThreatLR[[StrataL[i]]]$Agriculture_2.3a,
                                     Agriculture_2.3b=ThreatLR[[StrataL[i]]]$Agriculture_2.3b,
                                     Agriculture=ThreatLR[[StrataL[i]]]$Agriculture,
                                     Energy_3.1=ThreatLR[[StrataL[i]]]$Energy_3.1,
                                     Energy_3.2=ThreatLR[[StrataL[i]]]$Energy_3.2,
                                     Energy_3.3=ThreatLR[[StrataL[i]]]$Energy_3.3,
                                     Energy=ThreatLR[[StrataL[i]]]$Energy,
                                     Transport_4.1=ThreatLR[[StrataL[i]]]$Transport_4.1,
                                     Transport_4.2=ThreatLR[[StrataL[i]]]$Transport_4.2,
                                     Transportation=ThreatLR[[StrataL[i]]]$Transportation,
                                     BioUse_5.1=ThreatLR[[StrataL[i]]]$BioUse_5.1a,
                                     BioUse_5.1=ThreatLR[[StrataL[i]]]$BioUse_5.1b,
                                     BioUse_5.3=ThreatLR[[StrataL[i]]]$BioUse_5.3,
                                     BioUse=ThreatLR[[StrataL[i]]]$BioUse,
                                     HumanIntrusion_6=ThreatLR[[StrataL[i]]]$HumanIntrusion_6,
                                     HumanIntrusion=ThreatLR[[StrataL[i]]]$HumanIntrusion
  )
  #write_csv(ThreatLZR[[StratName]], file.path(dataOutDir,paste(StratName,'.csv',sep='')))
}
# write out the list of threat strata data frames to a multi-tab excel spreadsheet
WriteXLS(ThreatLZR, file.path(dataOutDir,paste('GBThreats.xls',sep='')),SheetNames=StrataL)

#Make a single data.frame using the most appropriate strata and write out for further evaluation
ThreatI<-data.frame(GBPU_Name=ThreatLR[['GBPUr']]$POPULATION_NAME,
                                   Residential_1=ThreatLR[['GBPUr_LFormFlat']]$Residential_1,
                                   Residential=ThreatLR[['GBPUr']]$Residential,
                                   Agriculture_2.1=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2.1,
                                   Agriculture_2.3a=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2.3a,
                                   Agriculture_2.3b=ThreatLR[['GBPUr_LFormFlat']]$Agriculture_2.3b,
                                   Agriculture=ThreatLR[['GBPUr']]$Agriculture,
                                   Energy_3.1=ThreatLR[['GBPUr']]$Energy_3.1,
                                   Energy_3.2=ThreatLR[['GBPUr']]$Energy_3.2,
                                   Energy_3.3=ThreatLR[['GBPUr']]$Energy_3.3,
                                   Energy=ThreatLR[['GBPUr']]$Energy,
                                   Transport_4.1=ThreatLR[['GBPUr_NonHab']]$Transport_4.1,
                                   Transport_4.2=ThreatLR[['GBPUr_NonHab']]$Transport_4.2,
                                   Transportation=ThreatLR[['GBPUr']]$Transportation,
                    BioUse_5.1a=ThreatLR[['GBPUr_NonHab']]$BioUse_5.1a,
                    BioUse_5.1b=ThreatLR[['GBPUr_NonHab']]$BioUse_5.1b,
                    BioUse_5.3=ThreatLR[['GBPUr_Forest']]$BioUse_5.3,
                                   BioUse=ThreatLR[['GBPUr']]$BioUse,
                                   HumanIntrusion_6=ThreatLR[['GBPUr_NonHab']]$HumanIntrusion_6,
                                   HumanIntrusion=ThreatLR[['GBPUr']]$HumanIntrusion)

WriteXLS(ThreatI, file.path(dataOutDir,paste('GBThreatsI.xls',sep='')))


#Function to summarize threat deciles or quartiles
ThreatSummary <- function(DF, Threat_A, Threat_NS) {
  DF %>%
    mutate(Decile = ntile(DF[[Threat_A]], 10)) %>% 
    mutate(N_Negligible = as.integer(DF[[Threat_NS]] == 'Negligible')) %>% 
    mutate(N_Negligible = as.integer(DF[[Threat_NS]] == 'Negligible')) %>% 
    mutate(N_Unknown = as.integer(DF[[Threat_NS]] == 'Unknown')) %>% 
    mutate(N_Low = as.integer(DF[[Threat_NS]] == 'Low')) %>% 
    mutate(N_Med = as.integer(DF[[Threat_NS]] == 'Med')) %>% 
    group_by(Decile) %>%
    dplyr::summarise(MeanT=mean(DF[[Threat_A]], na.rm=T), count=n(), Unknown=sum(N_Unknown), Negilible=sum(N_Negligible), Low=sum(N_Low), Med=sum(N_Med))
}

ThreatAVars <- c('Residential_1','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Energy_3.1','Energy_3.2','Energy_3.3','Transport_4.1','Transport_4.2','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6')
ThreatNSVars <- c('Residential','Agriculture','Agriculture','Agriculture','Energy','Energy','Energy','Transportation','Transportation','BioUse','BioUse','BioUse','HumanIntrusion')
nTvars<-length(ThreatAVars)

ThreatSum<-list()

i<-1
for (i in 1:nTvars) {
  TA<-paste(ThreatAVars[i],sep='')
  TNS<-paste(ThreatNSVars[i],sep='')
  ThreatSum[[i]]<-ThreatSummary(ThreatI, TA, TNS)
  }
WriteXLS(ThreatSum, file.path(dataOutDir,paste('GBThreatSum.xls',sep='')),SheetNames=ThreatAVars)

#output isolation data.frames
Isolation<-list()
IsoList<-c('overal','external','internal')
Isolation<-list(Isolation_overal,Isolation_external, Isolation_internal)
WriteXLS(Isolation, file.path(dataOutDir,paste('GBIsolation.xls',sep='')),SheetNames=IsoList)

# summarize isolation comparizon
IsolationS<-list()
IsolationS[[1]]<-Isolation_overal %>%
  group_by(Iso_code) %>%
  dplyr::summarise(count=n(), mean(Current_10km2), mean(Current_100km2), mean(Current_300km2), mean(Current_1000km2), mean(Natural_10km2), mean(Natural_100km2), mean(Natural_300km2), mean(Natural_1000km2))
IsolationS[[2]]<-Isolation_external %>%
  group_by(Iso_code) %>%
  dplyr::summarise(count=n(), mean(Current_10km2), mean(Current_100km2), mean(Current_300km2), mean(Current_1000km2), mean(Natural_10km2), mean(Natural_100km2), mean(Natural_300km2), mean(Natural_1000km2))
IsolationS[[3]]<-Isolation_internal %>%
  group_by(Iso_code) %>%
  dplyr::summarise(count=n(), mean(Current_10km2), mean(Current_100km2), mean(Current_300km2), mean(Current_1000km2), mean(Natural_10km2), mean(Natural_100km2), mean(Natural_300km2), mean(Natural_1000km2))
WriteXLS(IsolationS, file.path(dataOutDir,paste('GBIsolationS.xls',sep='')),SheetNames=IsoList)











ThreatSum[[i]]

TA<-'Residential_1'
TNS<-'Residential'

ThreatSummary(Threats, as.name(paste('Threats$',ThreatAVars[i],sep='')), paste('Threats$',ThreatNSVars[i],sep=''))

ThreatSum[[i]]<-ThreatSummary(Threats, TA, TNS)

new_column1 <- function(df,col_name,col1,col2){
  #Create new column col_name as sum of col1 and col2
  df[[col_name]] <- df[[col1]] + df[[col2]]
  df
}
#Assign each to a decile

ThreatSummary <- function(DF, Threat_A, Threat_NS) {
  DF %>% 
  mutate(N_Low = as.integer(T1_DF$Residential == 'Low'))
}

mutate(Decile = ntile(Threat_A,10)) %>% 
  
%>% 
  mutate(N_Negligible = as.integer(Threat_NS == 'Negligible')) %>% 
  mutate(N_Unknown = as.integer(Threat_NS == 'Unknown'))
%>% 
  mutate(N_Med = as.integer(Threat_NS == 'Med')) 
}  
  %>% 
  group_by(Decile) %>%
  dplyr::summarise(Res1=mean(Threat_A, na.rm=T), count=n(), Unknown=sum(N_Unknown), Negilible=sum(N_Negligible), Low=sum(N_Low), Med=sum(N_Med))
}

Threat_A<-'T1_DF$Residential_1'
Threat_NS<-'T1_DF$Residential'

ThreatSummary(T1_DF, T1_DF$Residential_1, T1_DF$Residential_NS)

ThreatSummary <- function(DF, Threat_A, Threat_NS) {
  
}

Threat_A<-'T1_DF$Residential_1'
Threat_NS<-'T1_DF$Residential_NS'


T1_DFs1 <- T1_DF %>% 
  mutate(Decile = ntile(Threat_A,10)) %>% 
  mutate(N_Negligible = as.integer(Threat_NS == 'Negligible')) %>% 
  mutate(N_Unknown = as.integer(Threat_NS == 'Unknown')) %>% 
  mutate(N_Low = as.integer(Threat_NS == 'Low')) %>% 
  mutate(N_Med = as.integer(Threat_NS == 'Med')) %>% 
  group_by(Decile) %>%
  dplyr::summarise(Res1=mean(Threat_A, na.rm=T), count=n(), Unknown=sum(N_Unknown), Negilible=sum(N_Negligible), Low=sum(N_Low), Med=sum(N_Med))


T1_DFs2 <- T1_DF %>% 
       mutate(Decile = ntile(T1_DF$Residential_1,10)) %>% 
       mutate(N_Unknown = as.integer(Residential_NS == 'Unknown')) %>% 
       mutate(N_Negligible = as.integer(Residential_NS == 'Negligible')) %>% 
       mutate(N_Low = as.integer(Residential_NS == 'Low')) %>% 
       mutate(N_Med = as.integer(Residential_NS == 'Med')) %>% 
       group_by(Decile) %>%
       dplyr::summarise(Res1=mean(Residential_1, na.rm=T), count=n(), Unknown=sum(N_Unknown), Negilible=sum(N_Negligible), Low=sum(N_Low), Med=sum(N_Med))


