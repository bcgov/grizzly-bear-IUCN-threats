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
