# Copyright 2019 Province of British Columbia
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

#Generate Table for SoE application - based on RankTable
RankTable <- data.frame(read_excel(path=file.path(dataOutDir,paste('Threat_Calc.xls',sep='')))) %>%
  dplyr::select(GBPU=GBPU_Name,Region,Female_Popn_2018=Adults,Trend, PopIso,CalcRank,CalcSRank, Rank_Number,Overal_Threat=Threat_Class,
                ResidentialCalc, AgricultureCalc, EnergyCalc, TransportationCalc,
                BioUseCalc, HumanIntrusionCalc, ClimateChangeCalc)
#RankTable$GBPU <- gsub("\\*", "", RankTable$GBPU)

ThreatI <- data.frame(read_excel(path=file.path(dataOutDir,paste('GBThreatsI.xls',sep='')))) %>%
  dplyr::select(GBPU=GBPU_Name, Residential_1a, Residential_1b, Agriculture_2.1,
  Agriculture_2.3b, Energy_3.1, Energy_3.2, Energy_3.3, Energy_3all,
  Transport_4.1, BioUse_5.1a, BioUse_5.1b, BioUse_5.3, HumanIntrusion_6, 
  ClimateChange_11)

#Merge 2 tables together
Threat <- RankTable %>%
  merge(ThreatI,  by='GBPU') 

#Table of reference points
ThreatBench <- data.frame(read_excel(path=file.path(dataOutDir,paste('ThreatBench.xls',sep='')))) %>%
  dplyr::filter(Threat %in% c('Residential_1a', 'Residential_1b', 'Agriculture_2.1', 'Agriculture_2.3b', 
                              "Energy_3.1", "Energy_3.2", "Energy_3.3",'Energy_3all',
                              'Transport_4.1', 'BioUse_5.1a', 'BioUse_5.1b', 'BioUse_5.3', 'HumanIntrusion_6',
                              'ClimateChange_11'))

#join benchmarks to each column and identify each threat that is over threshold
ThreatAVars <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3b',
                 'Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all','Transport_4.1',
                 'BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')

#Function to calc low, med, high fail for each threat
#Pass df (Threat), Threat (ThreatC)
ThreatFailFn <- function(DF, ThreatA) {
    DF %>%
    mutate(TLow = ifelse(DF[[ThreatA]] > 
           (dplyr::filter(ThreatBench, Threat == ThreatA)$Low), 1, 0)) %>%
    mutate(TMed = ifelse(DF[[ThreatA]] > 
           (dplyr::filter(ThreatBench, Threat == ThreatA)$Med), 2, 0)) %>%
    mutate(THigh = ifelse(DF[[ThreatA]] > 
           (dplyr::filter(ThreatBench, Threat == ThreatA)$High), 3, 0)) 
}
  
Class_LUT<-data.frame(ThreatClsNum=c(0,1,2,3),
                      ThreatCls=c('Negligible','Low','Medium','High'))

#Threat_1 %>% mutate(TLow = ifelse(Threat_1[[ThreatC]] > 
#              (dplyr::filter(ThreatBench, Threat == ThreatC)$Low), 'Low', 'Negligible')) %>%
#              dplyr::select(GBPU,ClimateChangeCalc,ClimateChange_11,ClimateChange_11_subTFail,TLow)

#cycle through each threat and assign new variable for each sub-threat indicating pass or fail (1,2,3; low, medium, high)
Threat_1<-Threat
for (i in 1:length(ThreatAVars)) {
    ThreatC<-ThreatAVars[i]
     Threat_1<-ThreatFailFn(Threat_1, ThreatC) %>%
       #Threat_1$ThreatCombo <- paste(Threat_1$TLow,Threat_1$TMed,Threat_1$THigh,sep='')
       #names(Threat_1)[names(Threat_1) == 'ThreatCombo'] <- paste(ThreatC,'_Fail',sep='')
       #Threat_1$ThreatClsNum <- pmax(Threat_1$TLow,Threat_1$TMed,Threat_1$THigh) %>%
     mutate(ThreatClsNum = pmax(TLow,TMed,THigh)) %>%
     left_join(Class_LUT,by='ThreatClsNum')
     names(Threat_1)[names(Threat_1) == 'ThreatCls'] <- paste(ThreatC,'_subTFail',sep='')
     #names(Threat_1)[names(Threat_1) == "TLow"] <- paste(ThreatC,'_LowF', sep='')
     #names(Threat_1)[names(Threat_1) == "TMed"] <- paste(ThreatC,'_MedF', sep='') 
     #names(Threat_1)[names(Threat_1) == "THigh"] <- paste(ThreatC,'_HighF', sep='')
     #names(Threat_1)[names(Threat_1) == 'ThreatCombo'] <- paste(ThreatC,'_Fail',sep='')
   }

#Pull original GBPU 2019 population data for final SoE table
GBPop<- data.frame(read_xls(path=file.path(GBdataOutDir,'GBPUpop.xls'))) %>%
  mutate(GBPU_Name=POPULATION_NAME) %>%
  mutate(PopnEst2018=pop2018)

GBPop_out<- GBPop %>%
  dplyr::select(GBPU=GBPU_Name, PopnEst2018)

#Generate a composite variable that describes sub-threat for each main threat as a 1 to 3 
#digit depending on the number of sub-threats
Threat_2 <- Threat_1 %>%
  merge(GBPop_out,  by='GBPU') %>%
  mutate(Residential_TFail = paste('S_',Residential_1a_subTFail,Residential_1b_subTFail,sep='')) %>%
  mutate(Agriculture_TFail = paste('S_',Agriculture_2.1_subTFail,Agriculture_2.3b_subTFail,sep='')) %>%
  mutate(Energy_TFail = paste('S_',Energy_3.1_subTFail,Energy_3.2_subTFail,Energy_3.2_subTFail,sep='')) %>%
  mutate(Transport_TFail = paste('S_',Transport_4.1_subTFail,sep='')) %>%
  mutate(BioUse_TFail = paste('S_',BioUse_5.1a_subTFail,BioUse_5.1b_subTFail,BioUse_5.3_subTFail,sep='')) %>%
  mutate(HumanIntrusion_TFail = paste('S_',HumanIntrusion_6_subTFail,sep='')) %>%
  mutate(ClimateChange_TFail = paste('S_',ClimateChange_11_subTFail,sep='')) %>%
  dplyr::select(c(GBPU, Region, PopnEst2018, Female_Popn_2018,Trend,PopIso, CalcRank, CalcSRank,Rank_Number, Overal_Threat,
    ResidentialCalc, Residential_1a, Residential_1a_subTFail,Residential_1b, Residential_1b_subTFail,
    AgricultureCalc, Agriculture_2.1, Agriculture_2.1_subTFail,Agriculture_2.3b, Agriculture_2.3b_subTFail, 
    EnergyCalc, Energy_3.1, Energy_3.1_subTFail, Energy_3.2, Energy_3.2_subTFail, Energy_3.3, Energy_3.3_subTFail, Energy_3all, Energy_3all_subTFail,
    TransportationCalc, Transport_4.1,  Transport_4.1_subTFail,
    BioUseCalc, BioUse_5.1a, BioUse_5.1a_subTFail,BioUse_5.1b, BioUse_5.1b_subTFail, BioUse_5.3, BioUse_5.3_subTFail, 
    HumanIntrusionCalc,  HumanIntrusion_6, HumanIntrusion_6_subTFail,
    ClimateChangeCalc, ClimateChange_11, ClimateChange_11_subTFail
    ))

WriteXLS(ThreatBench, file.path(dataOutDir,paste('ThreatBench_SoE.xls',sep='')))

WriteXLS(Threat_2, file.path(dataOutDir,paste('GrizzlyBear_2019_ConservationRanking_Results.xls',sep='')))
write_csv(Threat_2,file.path(dataOutDir,paste('GrizzlyBear_2019_ConservationRanking_Results.csv',sep='')))

#generate a csv of bear densities associated with GBPU line work
GBPU_LEH<-readRDS(file = 'tmp/GBPU_LEH')
  st_geometry(GBPU_LEH) <- NULL # remove geometry, coerce to data.frame
  GBPU_LEH <- dplyr::select(GBPU_LEH, GRIZZLY_BEAR_POP_UNIT_ID, GRIZZLY_BEAR_POPULATION_TAG, POPULATION_NAME,
  DISPLAY_NAME, WILDLIFE_MGMT_UNIT_ID, REGION_RESPONSIBLE, REGION_RESPONSIBLE_NAME,
  GAME_MANAGEMENT_ZONE_ID, GAME_MANAGEMENT_ZONE_NAME, FID_WMU_BC, GBPU_MU_LEH_uniqueID, 
  WMU_linkID, EST_POP_2018, EST_POP_DENSITY_2018, AREA_KM2, AREA_KM2_noWaterIce)

WriteXLS(GBPU_LEH, file.path(dataOutDir,paste('GBPU_LEH.xls',sep='')))



