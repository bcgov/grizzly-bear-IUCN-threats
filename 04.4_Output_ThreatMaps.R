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
library(ggspatial)

# Plot out final rank map
# Read in GBPU and threat assessment data
GBPU <- readRDS(file = 'tmp/GBPU')
ThreatCalc <- data.frame(read_excel(path=file.path(dataOutDir,paste('Threat_Calc.xls',sep=''))))

#Merge to a sf map
RankMap<-merge(GBPU, ThreatCalc, by.x='POPULATION_NAME', by.y='GBPU_Name')

#Make RankCode a factor
RankMap$RankCode_map<-factor(RankMap$Rank_Number)
# Range coordinates - not used
#mapRange1 <- c(range(st_coordinates(GBPU.AOI.spatial)[,1]),range(st_coordinates(GBPU.AOI.spatial)[,2]))

plot_title<-"Grizzly Bear - Conservation Management Concern"
plot_legend<-"Conservation\nConcern"

# Prepare bcmaps data for plotting
bc_neighbours <- bc_neighbours()

pdf(file=file.path(figsOutDir,"GB_Rank.pdf"))
ggplot() +
  geom_sf(data = bc_neighbours[bc_neighbours$iso_a2 == 'OC',], 
          col = 'light blue', fill = 'light blue') +
  geom_sf(data = bc_neighbours[bc_neighbours$postal == 'BC',], 
          col = 'light grey', fill = 'light grey') +
  geom_sf(data = bc_bound(), col = "black", 
          alpha = 0, size = 0.5) +
  geom_sf(data = RankMap, aes(fill = RankCode_map)) +
  scale_fill_brewer(palette="RdYlGn", direction =1, labels = c('Extreme','High','High','Medium','Medium','Low','Low','Very Low')) +
  theme(legend.title = element_text(size=12, color = "black", face="bold"),
        legend.justification=c(1,0), 
        legend.position=c(0.95, 0.50),  
        legend.background = element_blank(),
        legend.key = element_blank()) + 
  labs(fill = plot_legend) +
  labs(x=element_blank(), y = element_blank()) +
  geom_sf(data = RankMap, col = "black", alpha = 0, size = 0.5)+
  geom_sf_text(data=RankMap, size = 2, color = 'white', aes(label = POPULATION_NAME)) +
  annotation_scale(location = "bl", width_hint = 0.25, 
                   pad_x = unit(0.35, "in"), pad_y = unit(0.3, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line   = element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2))
dev.off()

# Plot out final rank map
# Read in GBPU and threat assessment data
GBPU <- readRDS(file = 'tmp/GBPU')
ThreatCalc <- data.frame(read_excel(path=file.path(dataOutDir,paste('Threat_Calc.xls',sep=''))))

#Merge to a sf map
RankMap<-merge(GBPU, ThreatCalc, by.x='POPULATION_NAME', by.y='GBPU_Name')

#Make RankCode a factor
RankMap$RankCode_Smap<-factor(floor(RankMap$Rank_Number))
# Range coordinates - not used
#mapRange1 <- c(range(st_coordinates(GBPU.AOI.spatial)[,1]),range(st_coordinates(GBPU.AOI.spatial)[,2]))

plot_title<-"Grizzly Bear - Conservation Concern"
plot_legend<-"Conservation\nConcern"
# Prepare bcmaps data for plotting
bc_neighbours <- bc_neighbours()

pdf(file=file.path(figsOutDir,"GB_SRank.pdf"))
ggplot() +
  geom_sf(data = bc_neighbours[bc_neighbours$iso_a2 == 'OC',], 
          col = 'light blue', fill = 'light blue') +
  geom_sf(data = bc_neighbours[bc_neighbours$postal == 'BC',], 
          col = 'light grey', fill = 'light grey') +
  geom_sf(data = bc_bound(), col = "black", 
          alpha = 0, size = 0.5) +
  geom_sf(data = RankMap, aes(fill = RankCode_Smap)) +
  scale_fill_brewer(palette="RdYlGn", direction =1, labels = c('Extreme','High','Medium','Low','Very Low')) +
  theme(legend.title = element_text(size=12, color = "black", face="bold"),
        legend.justification=c(1,0), 
        legend.position=c(0.95, 0.50),  
        legend.background = element_blank(),
        legend.key = element_blank()) + 
  labs(fill = plot_legend) +
  labs(x=element_blank(), y = element_blank()) +
  geom_sf(data = RankMap, col = "black", alpha = 0, size = 0.5)+
  geom_sf_text(data=RankMap, size = 2, color = 'white', aes(label = POPULATION_NAME)) +
  annotation_scale(location = "bl", width_hint = 0.25, 
                   pad_x = unit(0.35, "in"), pad_y = unit(0.3, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line   = element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2))
dev.off()

#Plot out overall threat
# Read in GBPU and threat assessment data
GBPU <- readRDS(file = 'tmp/GBPU')
ThreatCalc <- data.frame(read_excel(path=file.path(dataOutDir,paste('Threat_Calc.xls',sep=''))))

#Merge to a sf map
ThreatN <- data.frame(Threat_Class=c('VHigh','High','Medium','Low','Negligible'),
                      Threat_Class_N=c(1,2,3,4,5))
RankMap <- merge(GBPU, ThreatCalc, by.x='POPULATION_NAME', by.y='GBPU_Name') %>%
            left_join(ThreatN, by='Threat_Class')

#Make RankCode a factor
RankMap$RankCode_Tmap<-factor(floor(RankMap$Threat_Class_N))
#RankMap$RankCode_Smap<-factor(floor(RankMap$Threat_Class))
# Range coordinates - not used
#mapRange1 <- c(range(st_coordinates(GBPU.AOI.spatial)[,1]),range(st_coordinates(GBPU.AOI.spatial)[,2]))

plot_title<-"Grizzly Bear - Overall Threat"
plot_legend<-"Overall Threat"
# Prepare bcmaps data for plotting
bc_neighbours <- bc_neighbours()

pdf(file=file.path(figsOutDir,"GB_Threat.pdf"))
ggplot() +
  geom_sf(data = bc_neighbours[bc_neighbours$iso_a2 == 'OC',], 
          col = 'light blue', fill = 'light blue') +
  geom_sf(data = bc_neighbours[bc_neighbours$postal == 'BC',], 
          col = 'light grey', fill = 'light grey') +
  geom_sf(data = bc_bound(), col = "black", 
          alpha = 0, size = 0.5) +
  geom_sf(data = RankMap, aes(fill = RankCode_Tmap)) +
  scale_fill_brewer(palette="RdYlGn", direction =1,  labels = c('Very High','High','Medium','Low','Negligible')) +
  theme(legend.title = element_text(size=12, color = "black", face="bold"),
        legend.justification=c(1,0), 
        legend.position=c(0.95, 0.50),  
        legend.background = element_blank(),
        legend.key = element_blank(), 
        legend.text = element_text(colour = 'black', face = 'bold')) +   
  labs(fill = plot_legend) +
  labs(x=element_blank(), y = element_blank()) +
  geom_sf(data=RankMap, col = "black", alpha = 0, size = 0.5)+
  geom_sf_text(data=RankMap, size = 2, color = 'white', aes(label = POPULATION_NAME)) +
  annotation_scale(location = "bl", width_hint = 0.25, 
                   pad_x = unit(0.35, "in"), pad_y = unit(0.3, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line   = element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2))
dev.off()



################
# Plot out strata and threat maps -
ThreatAVars <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all','Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')
Threats<-c('Residential','Agriculture','Energy','Transportation','BioUse','HumanIntrusion','ClimateChange')

Threat_file <- file.path("tmp/ThreatBrick")
ThreatBrick <- readRDS(file = Threat_file)

for (i in 1: nlayers(ThreatBrick)) {
  pdf(file=file.path(figsOutDir,paste(names(ThreatBrick)[i],"_ThreatMap.pdf",sep="")))
  plot(ThreatBrick[[i]])
  #lines(GBPU, col="red")
  dev.off()
}

StrataL <- c('GBPUr','GBPUr_NonHab','GBPUr_BEI_1_2','GBPUr_BEI_1_5','GBPUr_LFormFlat','GBPUr_Forest')
for (i in 1: length(StrataL)) {
  GBStrata<-raster(file.path(StrataDir,paste(StrataL[i], ".tif", sep="")))
  
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
  #lines(GBPU, col="red")
  dev.off()
}

  
