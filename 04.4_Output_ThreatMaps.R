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
library(GISTools)
library(RColorBrewer)

ThreatAVars <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Agriculture_2all','Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all','Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')
Threats<-c('Residential','Agriculture','Energy','Transportation','BioUse','HumanIntrusion','ClimateChange')

Threat_file <- file.path("tmp/ThreatBrick")
ThreatBrick <- readRDS(file = Threat_file)

ThreatCalc <- data.frame(read_excel(path=file.path(dataOutDir,paste('Threat_Calc.xls',sep=''))))
GBPU<-readOGR(dsn=file.path(DataDir,'GISData'), layer='GBPU')

for (i in 1: nlayers(ThreatBrick)) {
  pdf(file=file.path(figsOutDir,paste(names(ThreatBrick)[i],"_ThreatMap.pdf",sep="")))
  plot(ThreatBrick[[i]])
  lines(GBPU, col="red", lwd==50)
  dev.off()
}

StrataL <- c('GBPUr','GBPUr_NonHab','GBPUr_BEI_1_2','GBPUr_BEI_1_5','GBPUr_LFormFlat','GBPUr_Forest')
for (i in 1: length(StrataL)) {
  GBStrata<-raster(file.path(DataDir,'Strata',paste(StrataL[i], ".tif", sep="")))
  
  ## Slower than reclassify
  #ptm <- proc.time()
   #GBStrata[GBStrata[]>0 & GBStrata[]<1000]<-1
   #GBStrata[!GBStrata[]==1]<-NA
  #proc.time() - ptm 
  
  reclCls<-c(0,0,0,1,1000,1, 1000,5000,NA)
  recl<-matrix(reclCls,ncol=3,byrow=TRUE)
  GBrecl<-reclassify(GBStrata, rcl=recl, right=TRUE, include.lowest=TRUE)

  pdf(file=file.path(figsOutDir,paste(StrataL[i],"_StrataMap.pdf",sep="")))
  plot(GBrecl, col='green',main=StrataL[i],axes=FALSE,legend=FALSE)
  # plot(GBrecl,main=StrataL[i],axes=FALSE,legend=FALSE)
  lines(GBPU, col="red", lwd==50)
  dev.off()
}

#Set the timer
#ptm <- proc.time()

#proc.time() - ptm 
#gc()

#Plot out final rank map
RankMap<-merge(GBPU, ThreatCalc, by.x='POPULATION', by.y='GBPU_Name')

#Plotting
par(mar=c(3.1,3.1,1.1,1.1))
pdf(file=file.path(figsOutDir,paste("GBPU_Rank2.pdf",sep="")))
plotvar2<-(RankMap$RankCode)
plotclr<-(c("M1","M1M2","M2","M2M3","M3","M3M4","M4","M4M5","M5"))
nclr<-length(plotclr)
#names(plotclr) <- c('orange2','orange2','orange2','yellow2','yellow2','green2','green4','green4','green4')
names(plotclr) <- c((brewer.pal(nclr,"RdYlGn")))

match.idx <- match(plotvar2, plotclr)
colcode <- ifelse(is.na(match.idx), plotvar2, names(plotclr)[match.idx])

plot(RankMap, col=colcode)
lines(GBPU, col="red", lwd==50)

legend("topright", legend=c("M1","M1M2","M2","M2M3","M3","M3M4","M4","M4M5","M5"), fill=c((names(plotclr))), cex=0.8, title="Grizzly Bear-Status") #bty="n", bg='white'',

dev.off()
