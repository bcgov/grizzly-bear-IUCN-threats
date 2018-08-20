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

#Function to summarize threat deciles or quartiles
ThreatSummary <- function(DF, Threat_A, Threat_NS) {
  DF %>%
    mutate(Decile = ntile(DF[[Threat_A]], 10)) %>% 
    mutate(MT = (DF[[Threat_A]])) %>%
    mutate(N_Negligible = as.integer(DF[[Threat_NS]] == 'Negligible')) %>% 
    mutate(N_Negligible = as.integer(DF[[Threat_NS]] == 'Negligible')) %>% 
    mutate(N_Unknown = as.integer(DF[[Threat_NS]] == 'Unknown')) %>% 
    mutate(N_Low = as.integer(DF[[Threat_NS]] == 'Low')) %>% 
    mutate(N_Med = as.integer(DF[[Threat_NS]] == 'Med')) %>% 
    group_by(Decile) %>%
    dplyr::summarise(count=n(), MeanThreat=round(mean(MT),4), Unknown=sum(N_Unknown), Negilible=sum(N_Negligible), Low=sum(N_Low), Med=sum(N_Med))
  #Adplyr::summarise(MT=mean(), count=n(), Unknown=sum(N_Unknown), Negilible=sum(N_Negligible), Low=sum(N_Low), Med=sum(N_Med))
}

ThreatAVars <- c('Residential_1','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Agriculture_2all','Energy_3.1','Energy_3.2','Energy_3.3','Transport_4.1','Transport_4.2','Transport_4all','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6')
#ThreatAVars <- c('Residential_1Rank','Agriculture_2.1Rank','Agriculture_2.3aRank','Agriculture_2.3bRank','Energy_3.1','Energy_3.2','Energy_3.3','Transport_4.1Rank','Transport_4.2Rank','BioUse_5.1aRank','BioUse_5.1bRank','BioUse_5.3Rank','HumanIntrusion_6Rank')
ThreatNSVars <- c('Residential','Agriculture','Agriculture','Agriculture','Agriculture','Energy','Energy','Energy','Transportation','Transportation','Transportation','BioUse','BioUse','BioUse','HumanIntrusion')
nTvars<-length(ThreatAVars)

ThreatSumDecile<-list()

i<-1
for (i in 1:nTvars) {
  TA<-paste(ThreatAVars[i],sep='')
  TNS<-paste(ThreatNSVars[i],sep='')
  ThreatSumDecile[[i]]<-ThreatSummary(ThreatI, TA, TNS)
}

WriteXLS(ThreatSumDecile, file.path(dataOutDir,paste('GBThreatSumDecile.xls',sep='')),SheetNames=ThreatAVars)

