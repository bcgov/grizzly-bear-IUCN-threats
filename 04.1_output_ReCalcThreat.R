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

#
# Reads ThreatBench.xls from 03.4_analysis_ID_Thresholds.R and brings in GBThreatsI.xls
# Generates Threats_O.xls which has asssinged threat level for each GBPU
#

source("header.R")

ThreatAVars <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Agriculture_2all','Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all','Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')
ThreatNSVars <- c('Residential','Residential','Agriculture','Agriculture','Agriculture','Agriculture','Energy','Energy','Energy','Energy','Transportation','Transportation','Transportation','Transportation','Transportation','BioUse','BioUse','BioUse','HumanIntrusion','ClimateChange')

#######
# Using numerically derived benchmarks re-calculate the Threats for each GBPU
ThreatBench <- data.frame(read_excel(path=file.path(dataOutDir,paste('ThreatBench.xls',sep=''))))
ThreatI <- data.frame(read_excel(path=file.path(dataOutDir,paste('GBThreatsI.xls',sep=''))))

ThreatAssignFn <- function(DF, ThreatVal, ThreatCalc) {
  DF %>% 
    mutate(ReCalc = (DF[[ThreatVal]] >= ThreatCalc))
}

nThreatLevels<-2 #only Low and Medium
ThreatLevelsNames<-c('Low','Medium', 'High', 'VHigh')
nTvars<-length(ThreatAVars)# number of threats

j<-1
ThreatLevels<-list()
for (j in 1:nThreatLevels) {
  ThreatReCalc<-list()
  i<-1
  for (i in 1:nTvars) {
    ThreatValue<-paste(ThreatAVars[i],sep='')#ThreatVal
    ThreatCalculated<-ThreatBench[ThreatBench$Threat==ThreatValue,(j+1)]#ThreatCalc move to different column for each threat level
    ThreatReCalc[[i]] <- ThreatAssignFn(ThreatI, ThreatValue, ThreatCalculated)$ReCalc
  }  
  #ThreatLevels[[j]]<-ThreatReCalc
  #}
  
  # cast the ThreatReCalc list to a data.frame
  DFin<-data.frame(do.call(cbind, ThreatReCalc))
  cols <- sapply(DFin, is.logical)
  DFin[,cols] <- lapply(DFin[,cols], as.numeric)
  # set NA to 0
  DFin[is.na(DFin)] <- 0
  #bind GBPU names to data.frame and assign threat name to column
  Threat_DFF <- cbind(data.frame(ThreatI$GBPU_Name), DFin)
  colnames(Threat_DFF)<-c('GBPU_Name',paste(ThreatAVars,'_calc',sep=''))
  #colnames(Threat_DFF)<-c('GBPU_Name',paste(ThreatAVars,'_calcL',sep=''))
  
  #Build Threat data base using calculated values
  #for multiple threat category threats set if any one of them is flagged
  Threat_1<-
    Threat_DFF %>%
    #mutate(Threat_2 = ifelse((Agriculture_2.3b_calc + Agriculture_2all_calc) > 0, 1, 0)) %>% # Dropped range - eg Spatzi gets flagged due to large range tentures
    mutate(Threat_2 = ifelse((Agriculture_2.3b_calc + Agriculture_2.1_calc) > 0, 1, 0)) %>%
    mutate(Threat_5 = ifelse((BioUse_5.1a_calc + BioUse_5.1b_calc + BioUse_5.3_calc)>0, 1, 0)) %>%
    mutate(Threat_1 = ifelse((Residential_1a_calc + Residential_1b_calc)>0, 1, 0)) %>%
    #dplyr::rename(Threat_1 = Residential_1_calc) %>%
    dplyr::rename(Threat_3 = Energy_3all_calc) %>%
    #dplyr::rename(Threat_4 = Transport_4all_calc) %>% # includes seismic lines
    dplyr::rename(Threat_4 = Transport_4.1_calc) %>%
    dplyr::rename(Threat_6 = HumanIntrusion_6_calc) %>%
    dplyr::rename(Threat_11 = ClimateChange_11_calc) %>%
    dplyr::select(GBPU_Name,starts_with('Threat'))
  
  Threat_1$numT<-rowSums(Threat_1[2:7])
  
  ThreatLevels[[j]]<-Threat_1
}
names(ThreatLevels) <- ThreatLevelsNames[1:nThreatLevels]

#Generate a threat table showing threat by GBPU
ThreatAdd<-ThreatLevels[['Low']][2:8]+ThreatLevels[['Medium']][2:8]


ThreatSummaryL <- ThreatAdd %>%
  mutate(GBPU_Name = ThreatLevels$Low$GBPU_Name) %>%
  mutate(ResidentialCalc = ifelse(Threat_1==0, 'Negligible', 
                                  ifelse(Threat_1==1, 'Low',
                                         ifelse(Threat_1==2, 'Medium','Unknown')))) %>%
  mutate(AgricultureCalc = ifelse(Threat_2==0, 'Negligible', 
                                  ifelse(Threat_2==1, 'Low',
                                         ifelse(Threat_2==2, 'Medium','Unknown')))) %>%
  mutate(EnergyCalc = ifelse(Threat_3==0, 'Negligible', 
                             ifelse(Threat_3==1, 'Low',
                                    ifelse(Threat_3==2, 'Medium','Unknown')))) %>%
  mutate(TransportationCalc = ifelse(Threat_4==0, 'Negligible', 
                                     ifelse(Threat_4==1, 'Low',
                                            ifelse(Threat_4==2, 'Medium','Unknown')))) %>%
  mutate(BioUseCalc = ifelse(Threat_5==0, 'Negligible', 
                             ifelse(Threat_5==1, 'Low',
                                    ifelse(Threat_5==2, 'Medium','Unknown')))) %>%
  mutate(HumanIntrusionCalc = ifelse(Threat_6==0, 'Negligible', 
                                     ifelse(Threat_6==1, 'Low',
                                            ifelse(Threat_6==2, 'Medium','Unknown')))) %>%
  mutate(ClimateChangeCalc = ifelse(Threat_11==0, 'Negligible', 
                                    ifelse(Threat_11==1, 'Low',
                                           ifelse(Threat_11==2, 'Medium','Unknown')))) %>%
  dplyr::select(GBPU_Name,ends_with('Calc'))

WriteXLS(ThreatSummaryL, file.path(dataOutDir,paste('ThreatCalcSummary.xls',sep='')))

#Adjust lower threat levels to 0 if registering a higher level (med) threat so calculator works
ThreatLevels[[1]]<-data.frame(GBPU_Name=ThreatLevels[[1]][1], (abs(ThreatLevels[[2]][2:8]-1)*ThreatLevels[[1]][2:8]),ThreatLevels[[1]][9])

#Write out ThreatLevels to inspect
WriteXLS(ThreatLevels, file.path(dataOutDir,paste('GBThreatLevels.xls',sep='')),SheetNames=names(ThreatLevels))

#Overall threat level assignment
#Threat_O<- data.frame(GBPU_Name=ThreatLevels[[Low]]$GBPU_Name, NumLowT=ThreatLevels[[Low]]$numT, NumLowM=ThreatLevels[['Medium']]$numT)
#Generate overall threat class data base based on number of threat classes
Threat_O<-data.frame(matrix(0,ncol=0,nrow=nrow(ThreatLevels[[1]])))
for (j in 1:nThreatLevels) {
  Threat_O<-cbind(Threat_O, data.frame(Num=ThreatLevels[[j]]$numT))
}
#temp fix for medium threat from Threat_5 - data base error
#Threat_O[,2]<-0
#Threat_O[55,2]<-1 #for the Yahk

#append 0s for threats not included so logic is clean
BlankT<-data.frame(matrix(0,ncol=(4-nThreatLevels),nrow=nrow(Threat_O)))
Threat_O<-cbind(GBPU_Name=ThreatLevels[[1]]$GBPU_Name, Threat_O, BlankT)
colnames(Threat_O)<-c('GBPU_Name',ThreatLevelsNames)

#Cacluate overall threat
Threat_LUT<-data.frame(Threat_Class = c('Null','Low','Medium','High','VHigh'),
                       ThreatAdj = c(0,0,-1,-1,-2))
Threat_O<-Threat_O %>%
  mutate(Threat_Class = ifelse((Low>0 & Low<4), 'Low', 
                               ifelse(((Low>3 & Medium==0) | (High==0 & Medium==1 & Low<3)), 'Medium',
                                      ifelse(((High==1) | (High==0 & Medium>2) | (High==0 & Medium==2 & Low>1 ) | (High==0 & Medium==1 & Low>2)), 'High',
                                             ifelse((VHigh>0 | High>1 | (High==1 & Medium>1)),'VHigh','Null'))))) %>%
  left_join(Threat_LUT, by='Threat_Class')

WriteXLS(Threat_O, file.path(dataOutDir,paste('Threat_O.xls',sep='')))
