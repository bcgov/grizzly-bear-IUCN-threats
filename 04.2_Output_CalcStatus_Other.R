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


#### Other Code  #####
df<-Threat_2
new <- df  # create a copy of df
# using lapply, loop over columns and match values to the look up table. store in "new".
new[] <- lapply(df, function(x) df$Iso[match(x, Iso_LUT$IsoCode)])

new[] <- lapply(df, function(x) {
  Pop_LUT$PopCode[match(x, Pop_LUT$PopMin) & ]
  return(x)
})





lapply(xx, function(x){ 
  names(x) <- gsub("^recnum$", "ID", names(x))
  return(x)
})






if Ranking$Adults
Ranking %>%
  (AdultRank=Adults
   AdultRank= adults>PopMin & adults<PopMax
   
   terms<-data.frame(Ranking$Adults)
   key.match<-
     lookup(terms, key.match, key.reassign, missing = NA)
   
   
   
   data <- apply(data, 2, function(x) {x <- recode(x,"1=777; 0=888"); x})
   data <- sample(c("a", "b", "c", "d"), 10, replace = T) 
   lookup <- list(a = "Apple", b = "Pear") 
   dplyr::recode(data, lookup)
   
   data<-list(pop=c(0,10,10,100,250))
   popLUT<-list(A=0,B=100)
   
   do.call(dplyr::recode, c(data, popLUT))
   
   
   PopAssingFn <- function(DF, )
     
     ThreatAssignFn <- function(DF, ThreatVal, ThreatCalc) {
       DF %>% 
         mutate(ReCalc = (DF[[ThreatVal]] >= ThreatCalc))
     }
   
   ThreatReCalc<-list()
   i<-1
   for (i in 1:nTvars) {
     ThreatValue<-paste(ThreatAVars[i],sep='')#ThreatVal
     ThreatCalculated<-ThreatBench[ThreatBench$Threat==ThreatValue,(j+1)]#ThreatCalc move to different column for each threat level
     ThreatReCalc[[i]] <- ThreatAssignFn(ThreatI, ThreatValue, ThreatCalculated)$ReCalc
   }  
   
   
   
   
   ThreatAssignFn <- function(DF, ThreatVal, ThreatCalc) {
     DF %>% 
       mutate(ReCalc = (DF[[ThreatVal]] >= ThreatCalc))
   }
   
   nThreatLevels<-2 #only Low and Medium
   ThreatLevelsNames<-c('Low','Medium', 'High', 'VHigh')
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
     
     DFin<-data.frame(do.call(cbind, ThreatReCalc))
     cols <- sapply(DFin, is.logical)
     DFin[,cols] <- lapply(DFin[,cols], as.numeric)
     DFin[is.na(DFin)] <- 0
     Threat_DFF <- cbind(data.frame(ThreatI$GBPU_Name), DFin)
     colnames(Threat_DFF)<-c('GBPU_Name',paste(ThreatAVars,'_calc',sep=''))
     #colnames(Threat_DFF)<-c('GBPU_Name',paste(ThreatAVars,'_calcL',sep=''))
     
     #Build Threat data base
     Threat_1<-
       Threat_DFF %>%
       mutate(Threat_2 = ifelse((Agriculture_2.3b_calc + Agriculture_2all_calc) > 0, 1, 0)) %>%
       mutate(Threat_5 = ifelse((BioUse_5.1a_calc + BioUse_5.1b_calc + BioUse_5.3_calc)>0, 1, 0)) %>%
       dplyr::rename(Threat_1 = Residential_1_calc) %>%
       dplyr::rename(Threat_3 = Energy_3all_calc) %>%
       dplyr::rename(Threat_4 = Transport_4all_calc) %>%
       dplyr::rename(Threat_6 = HumanIntrusion_6_calc) %>%
       dplyr::select(GBPU_Name,starts_with('Threat'))
     
     Threat_1$numT<-rowSums(Threat_1[2:7])
     
     ThreatLevels[[j]]<-Threat_1
   }
   names(ThreatLevels) <- ThreatLevelsNames[1:nThreatLevels]
   
   #Overall threat level assignment
   #Threat_O<- data.frame(GBPU_Name=ThreatLevels[[Low]]$GBPU_Name, NumLowT=ThreatLevels[[Low]]$numT, NumLowM=ThreatLevels[['Medium']]$numT)
   #Generate overall threat class data base based on number of threat classes
   Threat_O<-data.frame(matrix(0,ncol=0,nrow=nrow(ThreatLevels[[1]])))
   for (j in 1:nThreatLevels) {
     Threat_O<-cbind(Threat_O, data.frame(Num=ThreatLevels[[j]]$numT))
   }
   #append 0s for threats not included so logic is clean
   BlankT<-data.frame(matrix(0,ncol=(4-nThreatLevels),nrow=nrow(Threat_O)))
   Threat_O<-cbind(GBPU_Name=ThreatLevels[[Low]]$GBPU_Name, Threat_O, BlankT)
   colnames(Threat_O)<-c('GBPU_Name',ThreatLevelsNames)
   
   #Cacluate overall threat
   Threat_O$Class<-'No'
   Threat_O$Class<-ifelse((Threat_O$Low>0 & Threat_O$Low<4),'Low','LowUnass')
   Threat_O$Class<-ifelse((Threat_O$Low>3 | Threat_O$Medium==1),'Medium',Threat_O$Class)
   Threat_O$Class<-ifelse((Threat_O$High>0 | Threat_O$Medium>2 | Threat_O),'Medium',Threat_O$Class)
   
   Threat_O %>%
     mutate(Class = ifelse((Low>0 & Low<4), 'Low', 
                           ifelse((Low>3 | Medium==1), 'Medium',
                                  ifelse((High>0 | Medium>2 | (Medium ==2 & Low==2) | (Medium==1 & Low>2)), 'High',
                                         ifelse((VHigh>1 | High>1 | (High==1 & Medium>1)),'VHigh','Null')))))
   
   
   ############
   ThreatAVars <- c('Residential_1','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Agriculture_2all','Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all','Transport_4.1','Transport_4.2','Transport_4all','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6')
   #ThreatAVars <- c('Residential_1Rank','Agriculture_2.1Rank','Agriculture_2.3aRank','Agriculture_2.3bRank','Energy_3.1','Energy_3.2','Energy_3.3','Transport_4.1Rank','Transport_4.2Rank','BioUse_5.1aRank','BioUse_5.1bRank','BioUse_5.3Rank','HumanIntrusion_6Rank')
   ThreatNSVars <- c('Residential','Agriculture','Agriculture','Agriculture','Agriculture','Energy','Energy','Energy','Energy','Transportation','Transportation','Transportation','BioUse','BioUse','BioUse','HumanIntrusion')
   nTvars<-length(ThreatAVars)
   
   #Read in compiled Threats from spreadsheet exported by 04_output_Integrate script
   ThreatI <- data.frame(read_excel(path=file.path(dataOutDir,paste('GBThreatsI.xls',sep=''))))
   
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
   
   #Convert list of Threats into a data.frame and pull out the low and med thresholds for each threat
   Threat_DF<-ldply(ThreatThreshold,data.frame)
   DF1<-merge(Threat_DF[Threat_DF$Rank=='Low',],Threat_DF[Threat_DF$Rank=='Medium',], by='Threat', all.x=TRUE)
   ThreatBench<-data.frame(Threat=DF1$Threat, Low=DF1$Quant1.x, Med=DF1$Quant1.y)
   WriteXLS(ThreatBench, file.path(dataOutDir,paste('ThreatBench.xls',sep='')))
   
   
   
   ### other code
   
   ThreatThreshold[ThreatThreshold$ThreatCat == 'Low',]$Quant1
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