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

Threats<-c('Residential','Agriculture','Energy','Transportation','BioUse','HumanIntrusion','ClimateChange')

#######
# Read in the final calc and the raw values - merge and output
ThreatCompare <- data.frame(read_excel(path=file.path(dataOutDir,paste('GBThreatCompare.xls',sep=''))))

#Only include numeric values
ThreatCompareBB<-ThreatCompare[ , !(names(ThreatCompare) %in% ThreatNSVars)]
ThreatCompareVars<-names(ThreatCompareBB)
ThreatCalcL<-ThreatCompareVars[grep('Calc',ThreatCompareVars)]
ThreatCompare<-ThreatCompareBB[ , !(names(ThreatCompareBB) %in% (ThreatCalcL))]
ThreatNames<-names(ThreatCompare)

  
#for each Threat
for (i in 1:length(Threats)) {
  ThreatValsL <- ThreatNames[grep(Threats[i], ThreatNames)]
  
  #for each threat variable
  for (j in 1:length(ThreatValsL)) {
    ThreatValue<-ThreatCompare[,ThreatValsL[j]]
    
    pdf(file=file.path(figsOutDir,paste(ThreatValsL[j],"_Graph.pdf",sep="")))
    #plot(density(ThreatValue,main=ThreatValsL[j])
    hist(ThreatValue,main=ThreatValsL[j], breaks=10,col='gray', border="white")
    dev.off()
     
    # Density plot
    par(mfrow=c(3, 3))
    colnames <- dimnames(crime.new)[[2]]
    for (i in 2:8) {
      d <- density(crime.new[,i])
      plot(d, type="n", main=colnames[i])
      polygon(d, col="red", border="gray")
    }
    
    
    
  }
}

for (i in 1:length(Threats)) {
  ThreatValsL <- ThreatNames[grep(Threats[i], ThreatNames)]
  ThreatCat<-ThreatCompareBB[,ThreatCalcL[i]]
  
  #for each threat variable
  for (j in 1:length(ThreatValsL)) {
    ThreatValue<-ThreatCompareBB[,ThreatValsL[j]]
    
    Res<-data.frame(ThreatCat=ThreatCat ,ThreatValue=ThreatCompareBB[,ThreatValsL[j]])
    
    pdf(file=file.path(figsOutDir,paste(ThreatValsL[j],"_Box.pdf",sep="")))
    Tbp<-boxplot(split(Res$ThreatValue,Res$ThreatCat),main=ThreatValsL[j],varwidth=T)
    mtext(side=1, line =2, at = 1:nlevels(Res$ThreatCat), paste("n=",Tbp$n, sep=""))
    dev.off()
    
  }
}

#Basic Stats

pdf(file=file.path(figsOutDir,paste(ThreatValsL[j],"_Stats.pdf",sep="")))
ResStat<-Res %>% 
  group_by(ThreatCat) %>% 
  dplyr::summarise(count=n(), Mean=mean(ThreatValue), SD=sd(ThreatValue), Max=max(ThreatValue), Quant1=quantile(ThreatValue)[2])
ResStat
dev.off()

shapiro.test(ThreatValue)
