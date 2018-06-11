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

############
ThreatAVars <- c('Residential_1','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Agriculture_2all','Energy_3.1','Energy_3.2','Energy_3.3','Transport_4.1','Transport_4.2','Transport_4all','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6')
#ThreatAVars <- c('Residential_1Rank','Agriculture_2.1Rank','Agriculture_2.3aRank','Agriculture_2.3bRank','Energy_3.1','Energy_3.2','Energy_3.3','Transport_4.1Rank','Transport_4.2Rank','BioUse_5.1aRank','BioUse_5.1bRank','BioUse_5.3Rank','HumanIntrusion_6Rank')
ThreatNSVars <- c('Residential','Agriculture','Agriculture','Agriculture','Agriculture','Energy','Energy','Energy','Transportation','Transportation','Transportation','BioUse','BioUse','BioUse','HumanIntrusion')
nTvars<-length(ThreatAVars)

#Read in compiled Threats from spreadsheet exported by 04_output_Integrate script
ThreatI <- data.frame(read_excel(path=file.path(dataOutDir,paste('GBThreatsI.xls',sep=''))))

WriteXLS(ThreatI, file.path(dataOutDir,paste('GBThreatsI.xls',sep='')))


#Function to calculate stats for each indicator

ThreatThresholdFn <- function(DF, ThreatCat, ThreatValue) {
  DF %>% 
    mutate(TV=(DF[[ThreatValue]])) %>%
    group_by(Rank=DF[[ThreatCat]]) %>% 
    dplyr::summarise(Threat=ThreatValue, count=n(), Mean=mean(TV), SD=sd(TV), Max=max(TV), Quant1=quantile(TV)[2], Quant3=quantile(TV)[4])
}

# Identify the 1st quartile as numeric threshold for identifying risk class
ThreatThreshold<-list()

i<-1
for (i in 1:nTvars) {
  ThreatC<-paste(ThreatNSVars[i],sep='')#ThreatCat
  ThreatV<-paste(ThreatAVars[i],sep='')#ThreatVar
  ThreatThreshold[[i]]<-ThreatThresholdFn(ThreatI, ThreatC, ThreatV)
  
}
WriteXLS(ThreatThreshold, file.path(dataOutDir,paste('GBThreatThresholds.xls',sep='')),SheetNames=ThreatAVars)
#issues with energy - they are points - consider adding together, as well add agriculture + range
#check all values - especially points, linear, density - check standard thresholds and where the ranking falls.


### other code

ResStat[ResStat$ThreatCat == 'Low',]$Quant1
stringsAsFactors=TRUE
str(Res)

plot(x=Res$ThreatCat,y=Res$ThreatValue)

plot(density(Res$ThreatValue))

#test for normality
shapiro.test(Res$ThreatValue)

#calculate the mean, SD, max, Quant1 and 3 of the distribution for each category
ResStat<-Res %>% 
  group_by(ThreatCat) %>% 
  dplyr::summarise(count=n(), Mean=mean(ThreatValue), SD=sd(ThreatValue), Max=max(ThreatValue), Quant1=quantile(ThreatValue)[2], Quant3=quantile(ThreatValue)[4])

ResStat[ResStat$ThreatCat == 'Low',]$Quant1
SkeenaGBPU<-c('Babine','Bulkley-Lakes','Cranberry','Francois','Khutzeymateen','North Coast','Stewart','Upper Skeena-Nass')

Res<-data.frame(ThreatCat=ThreatI$Residential,ThreatValue=ThreatI$Residential_1)
Res<-data.frame(ThreatCat=ThreatI$Agriculture,ThreatValue=ThreatI$Agriculture_2.3a)
Res<-data.frame(ThreatCat=ThreatI$Agriculture,ThreatValue=ThreatI$Agriculture_2.3b)

SkeenaThreatI<-subset(ThreatI, GBPU_Name %in% SkeenaGBPU)
DF<-data.frame(SkeenaThreatI$GBPU_Name,SkeenaThreatI$Agriculture,SkeenaThreatI$Agriculture_2.3a,SkeenaThreatI$Agriculture_2.3b)
stringsAsFactors=TRUE
str(Res)

plot(x=Res$ThreatCat,y=Res$ThreatValue)

plot(density(Res$ThreatValue))

#test for normality
shapiro.test(Res$ThreatValue)

#calculate the mean, SD, max, Quant1 and 3 of the distribution for each category
ResStat<-Res %>% 
  group_by(ThreatCat) %>% 
  dplyr::summarise(count=n(), Mean=mean(ThreatValue), SD=sd(ThreatValue), Max=max(ThreatValue), Quant1=quanti