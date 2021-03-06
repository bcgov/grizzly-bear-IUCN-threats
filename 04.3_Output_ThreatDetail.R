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

ThreatAVars <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all','Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')
ThreatNSVars <- c('Residential','Residential','Agriculture','Agriculture','Agriculture','Energy','Energy','Energy','Energy','Transportation','Transportation','Transportation','Transportation','Transportation','BioUse','BioUse','BioUse','HumanIntrusion','ClimateChange')

nTvars<-length(ThreatAVars)

#######
# Read in the final calc and the raw values - merge and output
ThreatCalc<-data.frame(read_excel(path=file.path(dataOutDir,paste('Threat_Calc.xls',sep=''))))
ThreatI <- data.frame(read_excel(path=file.path(dataOutDir,paste('GBThreatsI.xls',sep=''))))

Threat<-
    ThreatCalc %>%
    left_join(ThreatI, by='GBPU_Name')


ThreatCompare<-data.frame(Threat$GBPU_Name,Threat$Region,Threat$Residential.x,Threat$Residential_1a,Threat$Residential_1b,Threat$ResidentialCalc,
                          Threat$Agriculture.x,Threat$Agriculture_2.1,Threat$Agriculture_2.3b,Threat$AgricultureCalc,
                          Threat$Energy.x,Threat$Energy_3all,Threat$EnergyCalc,
                          Threat$Transportation.x,Threat$Transport_4.1,Threat$TransportationCalc,
                          Threat$BioUse.x,Threat$BioUse_5.1a,Threat$BioUse_5.1b,Threat$BioUse_5.3,Threat$BioUseCalc,
                          Threat$HumanIntrusion.x,Threat$HumanIntrusion_6,Threat$HumanIntrusionCalc,
                          Threat$ClimateChange.x,Threat$ClimateChange_11,Threat$ClimateChangeCalc)
                          
ThreatColNames <- c('GBPU_Name','Region','Residential','Residential_1a','Residential_1b','ResidentialCalc',
                 'Agriculture','Agriculture_2.1','Agriculture_2.3b','AgricultureCalc',
                 'Energy','Energy_3all','EnergyCalc',
                 'Transportation','Transportation_4.1','TransportationCalc',
                 'BioUse','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','BioUseCalc',
                 'HumanIntrusion','HumanIntrusion_6','HumanIntrusionCalc',
                 'ClimateChange','ClimateChange_11','ClimateChangeCalc')

colnames(ThreatCompare)<-ThreatColNames

WriteXLS(ThreatCompare, file.path(dataOutDir,paste('GBThreatCompare.xls',sep='')))

# Output each threat with values and ranking of GBPU
Threats<-unique(ThreatNSVars)
ThreatList<-list()

ThreatList[[1]]<-ThreatCompare %>%
  dplyr::select(GBPU_Name,starts_with('Residential')) %>%
  #make a new variable baserd on 'Calc' variable
  filter(ResidentialCalc != 'Negligible')

ThreatList[[2]]<-ThreatCompare %>%
  dplyr::select(GBPU_Name,starts_with('Agriculture')) %>%
  #make a new variable baserd on 'Calc' variable
  filter(AgricultureCalc != 'Negligible')

ThreatList[[3]]<-ThreatCompare %>%
  dplyr::select(GBPU_Name,starts_with('Energy')) %>%
  #make a new variable baserd on 'Calc' variable
  filter(EnergyCalc != 'Negligible')

ThreatList[[4]]<-ThreatCompare %>%
  dplyr::select(GBPU_Name,starts_with('Transportation')) %>%
  #make a new variable baserd on 'Calc' variable
  filter(TransportationCalc != 'Negligible')

ThreatList[[5]]<-ThreatCompare %>%
  dplyr::select(GBPU_Name,starts_with('BioUse')) %>%
  #make a new variable baserd on 'Calc' variable
  filter(BioUseCalc != 'Negligible')

ThreatList[[6]]<-ThreatCompare %>%
  dplyr::select(GBPU_Name,starts_with('HumanIntrusion')) %>%
  #make a new variable baserd on 'Calc' variable
  filter(HumanIntrusionCalc != 'Negligible')

ThreatList[[7]]<-ThreatCompare %>%
  dplyr::select(GBPU_Name,starts_with('ClimateChange')) %>%
  #make a new variable baserd on 'Calc' variable
  filter(ClimateChangeCalc != 'Negligible')

WriteXLS(ThreatList, file.path(dataOutDir,paste('GBPU_threats.xls',sep='')),SheetNames=Threats)
