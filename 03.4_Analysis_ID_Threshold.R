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

#Reads GBThreatsI.xls and generates some simple stats for each threat 
#to help determing possible thresholds-GBThreatThresholds.xls & 
#summarizes possible thresholds along with CE derived-ThreatBench.xls

source("header.R")

############

ThreatAVars <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Agriculture_2all','Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all','Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')
ThreatNSVars <- c('Residential','Residential','Agriculture','Agriculture','Agriculture','Agriculture','Energy','Energy','Energy','Energy','Transportation','Transportation','Transportation','Transportation','Transportation','BioUse','BioUse','BioUse','HumanIntrusion','ClimateChange')
nTvars<-length(ThreatAVars)

#Read in compiled Threats from spreadsheet exported by 04_output_Integrate script
ThreatI <- data.frame(read_excel(path=file.path(dataOutDir,paste('GBThreatsI.xls',sep=''))))

#Function to calculate stats for each indicator

ThreatThresholdFn <- function(DF, ThreatCat, ThreatValue) {
  DF %>% 
    mutate(TV = (DF[[ThreatValue]])) %>%
    group_by(Rank=DF[[ThreatCat]]) %>% 
    dplyr::summarise(Threat=ThreatValue, count=n(), Mean=mean(TV), SD=sd(TV), Max=max(TV), Quant1=quantile(TV)[2], Quant3=quantile(TV)[4])
}

ThreatMaxFn <- function(DF, ThreatCat, ThreatValue) {
  DF %>%
    dplyr::summarise(Threat=ThreatValue, MaxAll=max(DF[[ThreatValue]]))
}

# Identify the 1st quartile as numeric threshold for identifying risk class
ThreatThreshold<-list()
ThreatMax<-list()

i<-1
for (i in 1:nTvars) {
  ThreatC<-paste(ThreatNSVars[i],sep='')#ThreatCat
  ThreatV<-paste(ThreatAVars[i],sep='')#ThreatVar
  ThreatThreshold[[i]]<-ThreatThresholdFn(ThreatI, ThreatC, ThreatV)
  ThreatMax[[i]]<-ThreatMaxFn(ThreatI, ThreatC, ThreatV)
}

WriteXLS(ThreatThreshold, file.path(dataOutDir,paste('GBThreatThresholds.xls',sep='')),SheetNames=ThreatAVars)

#Convert list of Threats into a data.frame and pull out the low and med thresholds for each threat
Threat_DF<-ldply(ThreatThreshold,data.frame)
ThreatMax_DF<-ldply(ThreatMax,data.frame)

DF1a<-merge(Threat_DF[Threat_DF$Rank=='Low',],Threat_DF[Threat_DF$Rank=='Medium',], by='Threat', all=TRUE)
DF1b<-merge(DF1a, Threat_DF[Threat_DF$Rank=='Unknown',], by='Threat', all=TRUE)
DF1<-merge(DF1b, ThreatMax_DF, by='Threat', all.x=TRUE)#Rank.x is Low, Rank.y is Medium, Rank is Unknown


# clean up some of the scores
# set medium (ie no medium scores) to max of all + 0.01 - it wont ever get flagged
DF1$Quant1.y<-ifelse(is.na(DF1$Quant1.y), DF1$MaxAll+0.01, DF1$Quant1.y)

# set low to 0.01 if 0 or NA - so not 0
DF1$Quant1.x<-ifelse((DF1$Quant1.x ==0| is.na(DF1$Quant1.x) ), 0.01, DF1$Quant1.x)
ThreatBenchI<-data.frame(Threat=DF1$Threat, LowIn=DF1$Quant1.x, MedIn=DF1$Quant1.y, UnknownIn=DF1$Quant1)

#Fix Threat 3 - it is a count and typically few resulting in a 0 - assign to 1
ThreatBenchI$LowIn[ThreatBenchI$Threat=='Energy_3all']<-1
#Add column of the benchmarks used Provincially for the Cumulative Effects current condition assessment
#add a 1.2 for roads as a medium threat
#Mortality - some units have <1% area flagged likely slivers so using CE criteria flag a GBPU if >1% is flagged - could be a single LU in a small number of cases
#Mortality update - flag if >50% of GBPU has mortality issues to temper the rating - check with Bear Team, flag 100% as medium
#Climate change - set at 25% or more -ve change in salmon biomas then flagged
CE_Bench<-data.frame(Threat=c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b',
                              'Agriculture_2all','Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all',
                              'Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1a',
                              'BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11'),
                     CE_BenchmarkL=c(0,0,0,0,0,0,0,0,0,0,0.6,0,0,0.6,0,50,1.508812,30,20,25),
                     CE_BenchmarkM=c(0,0,0,0,0,0,0,0,0,0,1.2,0,0,1.2,0,0,0,0,0,0))

ThreatBenchI<-left_join(ThreatBenchI,CE_Bench, by='Threat')
#Modify ThreatBench and use CE_Benchmarks where they are available
ThreatBench <- ThreatBenchI
ThreatBench<-
  ThreatBenchI %>% 
  mutate(Low = ifelse((ThreatBench$CE_BenchmarkL==0), ThreatBench$LowIn, ThreatBench$CE_BenchmarkL)) %>%
  mutate(Med = ifelse((ThreatBench$CE_BenchmarkM==0), ThreatBench$MedIn, ThreatBench$CE_BenchmarkM)) %>%
  mutate(Unknown = ThreatBench$UnknownIn) %>%
  dplyr::select(Threat,Low,Med,Unknown)

#Set NAs to near 0
ThreatBench$Unknown[is.na(ThreatBench$Unknown)]<-0

#CE Benchmarks Detail
#Residential area - 1 - no CE equivalent
#Agriculture area - 2.1 no CE equivalent
#Livestock Farming & Ranching area- 2.3a - no CE equivalent
#Livestock Farming & Ranching - 2.3b - cow density - no CE benchmark set
#Energy - 3 - no CE equivalent
#Road Density - 4.1, 4.2 (utility corridors) combined to 4all - linear disturbance
#CE uses line over poly calculation vs raster - >0.6km/km2
#Mortality - 5.1a
#Pop_Mort_Flag_Hunt - flag fail - mortality exceeded in one of 3 time periods in hunted units
#pass/fail - set at >0.1% of GBPU area to account for slivers
#Hunter Days- 5.1b:
#Low:  0-0.601977
#Mod:  0.601978 - 1.508812
#High:  > 1.508812 - Fail for CE
#Mid-Seral - 5.3
#%forested >30
#Encounter Class - 6
#>20% of LU in Front Country

WriteXLS(ThreatBench, file.path(dataOutDir,paste('ThreatBench.xls',sep='')))

