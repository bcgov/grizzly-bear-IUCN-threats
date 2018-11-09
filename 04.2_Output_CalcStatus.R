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

ThreatAVars <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Agriculture_2all','Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all','Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')
ThreatNSVars <- c('Residential','Residential','Agriculture','Agriculture','Agriculture','Agriculture','Energy','Energy','Energy','Energy','Transportation','Transportation','Transportation','Transportation','Transportation','BioUse','BioUse','BioUse','HumanIntrusion','ClimateChange')

#######
# Using numerically derived benchmarks re-calculate the Threats for each GBPU
Threat_O <- data.frame(read_excel(path=file.path(dataOutDir,paste('Threat_O.xls',sep=''))))
Ranking<-readRDS(file=file.path(DataDir,'Ranking'))
Ranking_full<-readRDS(file=file.path(DataDir,'Ranking_full'))
Ranking_fullSept<-readRDS(file=file.path(DataDir,'Ranking_fullSept'))

ThreatSummaryL<-data.frame(read_excel(path=file.path(dataOutDir,paste('ThreatCalcSummary.xls',sep=''))))

Ranking<-data.frame(GBPU_Name=Ranking_fullSept$GBPU_Name,
                    Adults=Ranking_fullSept$Adults, Iso=Ranking_fullSept$Iso, TrendAdj=Ranking_fullSept$TrendAdjNEW,OrigRanking=Ranking_fullSept$OrigRanking,
                    Residential=Ranking_fullSept$Residential,Agriculture=Ranking_fullSept$Agriculture, Energy=Ranking_fullSept$Energy, 
                    Transportation=Ranking_fullSept$Transportation, BioUse=Ranking_fullSept$BioUse,HumanIntrusion=Ranking_fullSept$HumanIntrusion,ClimateChange=Ranking_full$ClimateChange)

Iso_LUT<-data.frame(IsoCode = c('D','C','B','A'),
                    Iso = c('<25','25-66','66-90','>90'))

#From Proctor
IsoPopAdj_LUT<-data.frame(PopIso=c('AA','AB','AC','AD','BA','BB','BC','BD','CA','CB','CC','CD','DA','DB','DC','DD','EA','EB','EC','ED'),
                          PopIsoAdj=c(-4,-4,-4,-3,  -4,-1.5,-1,-0.5,  -4,-1.5,-1,0,  -3,-1,-0.5,0,  -2,-1,-0.5,0))

Rank_LUT<-data.frame(Rank=c('1','1.5','2','2.5','3','3.5','4','4.5','5'),
                     RankCode=c('M1','M1M2','M2','M2M3','M3','M3M4','M4','M4M5','M5'))

#Create a new table and populate the population codes, isolation codes, 
#combined population-isolation code, and rank score code
Threat_2<-
  Ranking %>%
  left_join(Threat_O, by='GBPU_Name') %>%
  mutate(PopCode = ifelse(Adults<10 , 'A', 
                          ifelse(Adults<50, 'B',
                                 ifelse(Adults<100, 'C', 
                                        ifelse(Adults<250, 'D', 'E'))))) %>% 
  left_join(Iso_LUT, by='Iso') %>%
  mutate(PopIso = paste(PopCode, IsoCode, sep=''))  %>%
  left_join(IsoPopAdj_LUT, by='PopIso') %>%
  mutate(Rank=as.character(
    ifelse((5+PopIsoAdj+TrendAdj+ThreatAdj)<=0, 1, (5+PopIsoAdj+TrendAdj+ThreatAdj))
  )) %>%
  left_join(Rank_LUT, by='Rank')  %>%
  left_join(ThreatSummaryL, by='GBPU_Name')

Threat_3<-Threat_2[,c(1,2,19,3,20,4,21:22,13:18,   23,24,5,  6,25,7,26,8,27,9,28,10,29,11,30,12,31)]
WriteXLS(Threat_3,file.path(dataOutDir,paste('Threat_Calc.xls',sep='')))

