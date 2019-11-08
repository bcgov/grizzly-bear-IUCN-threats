# Copyright 2019 Province of British Columbia
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
library(scales)
library(ggplot2)

ThreatAVars <- c('Residential_1a','Residential_1b','Agriculture_2.1','Agriculture_2.3a','Agriculture_2.3b','Energy_3.1','Energy_3.2','Energy_3.3','Energy_3all','Transport_4.1','Transport_4.1L','Transport_4.2L','Transport_4all','Transport_4allL','BioUse_5.1a','BioUse_5.1b','BioUse_5.3','HumanIntrusion_6','ClimateChange_11')
Threats<-c("ResidentialCalc", "AgricultureCalc", "EnergyCalc", "TransportationCalc",
           "BioUseCalc", "HumanIntrusionCalc", "ClimateChangeCalc")
Threat_file <- file.path("tmp/ThreatBrick")
ThreatBrick <- readRDS(file = Threat_file)

GBPU <- readRDS(file = 'tmp/GBPU')

ThreatCalc <- data.frame(read_excel(path=file.path(dataOutDir,paste('Threat_Calc.xls',sep=''))))
ThreatCalc[is.na(ThreatCalc)] <- 'Negligible'

#GBPU<-readOGR(dsn=file.path(GBspatialDir), layer='GBPU')

#Generate graph of Threat Class count
TTable<-ThreatCalc %>%
  group_by(Threat_Class) %>%
  #dplyr::filter(!(WMU %in% excludes)) %>%
  dplyr::summarise(count=n())

#Set Threat_Class as a factor and define the levels so appears in the correct order
TTable$Threat_Class <- factor(TTable$Threat_Class, levels=c("Negligible", "Low", "Medium", "High", "VHigh"))

pdf(file=file.path(figsOutDir,"GB_ThreatCount.pdf"))
ggplot(data=TTable, aes(x=Threat_Class, y=count, fill=Threat_Class)) +
  geom_bar(stat="identity", width = 0.75, position=position_dodge(width=11)) +
  #facet_grid(~Threat_Class, scales = 'free_x', space = 'free') +
  scale_fill_brewer(palette="RdYlGn", direction =-1) + 
  #geom_text(stat = 'count', aes(x=factor(Threat_Class),y=count,fill = factor(Threat_Class),
  #                              label = stat(count)))+#, position=position_stack(reverse=TRUE,0.5)) +
  #geom_text(stat='identity', aes(label= stat(count)), vjust=-1) +
  geom_text(aes(x = Threat_Class, y = count, label = count), hjust=1.2, size=7) +
  theme(legend.position="none") +
  labs(x="Overall Threat", y = "Number of Grizzly Bear Population Units") +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text=element_text(size=15)) +
  theme(legend.title=element_text(size=15)) +
  
  coord_flip()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks  = element_blank(),
        axis.line   = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2)) 
dev.off()

#Generate graph of # of GBPUs for each threat
#Threats<-grep("Calc", names(ThreatCalc), value=TRUE)

ThreatLevels<-list()

#Loop through make a list for each GBPU, Threat
for (i in 1:length(Threats)) {
  ThreatLevels[[i]]<-ThreatCalc %>% 
    dplyr::select(GBPU_Name, ThreatLevel = Threats[i]) %>%
    mutate(ThreatType = Threats[i])
}
#Row bind the list to make a GBPU, ThreatType, ThreatLevel table for graphing
TTable<-  bind_rows(ThreatLevels)

TTable$ThreatLevel <- factor(TTable$ThreatLevel, levels=c("VHigh", "High", "Medium", "Low", "Negligible"))
TTable$ThreatType <- factor(TTable$ThreatType, levels=rev(Threats))

pdf(file=file.path(figsOutDir,"GB_ThreatClassCount2.pdf"))
ggplot(data=TTable) +
  geom_bar(aes(x = factor(ThreatType), fill = ThreatLevel), 
           position = position_stack(reverse = TRUE), width = 0.75) +
    scale_fill_brewer(palette="RdYlGn", direction =1) +
  labs(fill = 'Threat Level') +
  scale_x_discrete(limit = rev(Threats),
                   labels = rev(c("Residential","Agriculture","Energy",
                              "Transportation","Bio Use","Human Intrusion",
                              "Climate Change"))) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,55)) +
  xlab("Threat") + ylab("Number of Grizzly Bear Population Units") +
  #ggtitle("Threat Impact to Grizzly Bear Population Units") +
  #geom_text(stat = 'count', aes(x=factor(ThreatType),#fill = factor(ThreatLevel),
   #        label = stat(count)), position=position_stack(reverse=TRUE,0.5),size=7) +
  #geom_text(aes(label='stat(count)'), stat='count', position='fill') +
  #geom_text(position = "stack", 
#            aes(x = factor(ThreatType), y = stat(count), 
#            ymax = stat(count), label = stat(count), hjust = 0.5)) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text=element_text(size=15)) +
  theme(legend.title=element_text(size=15)) +
  
  coord_flip() +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks  = element_blank(),
        axis.line   = element_line(colour=NA),
        panel.border = element_rect(colour = "black", fill=NA, size=2)) 
dev.off()

# Make a formated table of GBPUs for the report - showing past designation, round 1 ranking,
# calculated ranking
GBPU <- readRDS(file = 'tmp/GBPU')
ThreatI <- data.frame(read_excel(path=file.path(dataOutDir,paste('GBThreatsI.xls',sep=''))))

#Merge to a sf map
library(htmltools)
library(webshot)
library(formattable)


# formattable export function
export_formattable <- function(f, file, width = "100%", height = NULL,
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}
#(source: https://github.com/renkun-ken/formattable/issues/26)
#RankMap<-RankMap[order(RankMap$GBPU_Name),]

# Generate Table of results by Region
RankMap1<-GBPU %>%
  merge(ThreatCalc, by.x='POPULATION_NAME', by.y='GBPU_Name') %>%
  st_drop_geometry()


#RankMap1$POPULATION_NAME<-ifelse(RankMap1$POPULATION_NAME %in% c('Yahk','SouthSelkirk'), POPULATION_NAME)

#Merge in the original values for each threat to output
RankMap2<-RankMap1 %>%
  merge(ThreatI, by.x='POPULATION_NAME', by.y='GBPU_Name') %>%
  #flag the 2 GBPUs that have manually adjusted ranks, these are set in 4.2_output_CalcStatus.R
  mutate(POPULATION_NAME = ifelse(POPULATION_NAME %in% c('Yahk','South Selkirk'), 
                                  paste(POPULATION_NAME,'*',sep=''),POPULATION_NAME))

RankTable <- RankMap2 %>%
  dplyr::select(GBPU=POPULATION_NAME, Region, Female_Popn_2018=Adults, PopIso, 
                CalcRank, Rank_Number, Overal_Threat=Threat_Class, 
                ExpertRank, ExpertOverallThreat,
                ResidentialCalc,Residential_1a,Residential_1b,
                AgricultureCalc, Agriculture_2.1, Agriculture_2.3b,
                EnergyCalc,Energy_3all,
                TransportationCalc, Transport_4.1,
                BioUseCalc, BioUse_5.1a,BioUse_5.1b,BioUse_5.3,
                HumanIntrusionCalc, HumanIntrusion_6,
                ClimateChangeCalc, ClimateChange_11)
  
WriteXLS(RankTable, file.path(dataOutDir,paste('RankTable.xls',sep='')))

RankTableR <- RankMap2 %>%
  dplyr::select(GBPU=POPULATION_NAME, Female_Popn_2018=Adults, Rank_2012=STATUS,
                Rank=CalcSRank, Overal_Threat=Threat_Class)

WriteXLS(RankTableR, file.path(dataOutDir,paste('RankTableR.xls',sep='')))

#colnames(RankTable) <- c('GBPU','Female Pop 2018','Popn Iso','Overall Threat','Prev Status','Round 1 Rank','Revised Rank','RankNumber')
#RankTable$GBPU <- factor(RankTable$GBPU)

df.list<-list(RankTable)
DT.list<-list()
npages<-1
j<-1
for (j in 1:npages) {
  df<-df.list[[j]]
  rownames(df)<-NULL
  
  DT<-formattable(df[order(df$Rank_Number),], list(Rank_Number =FALSE,
    'GBPU'= formatter("span", style =
            ~ style(color = ifelse(Rank_Number > 3, "green", "red"))),
   # formattable::area(col = c('GBPU')) ~ color_tile = ifelse(Rank_Number > 3, "green", "red"),
    formattable::area(col = c('Female Popn_2018')) ~ normalize_bar("pink", 0.1),
    'PopnIso','Overall_Threat',
    'Prev_Status'= formatter("span", style =
            ~ style(color = ifelse(`Prev_Status` %in% c('Viable'), "green", "red"))),
    'Round_1_Rank'= formatter("span", style =
            ~ style(color = ifelse(`Round_1_Rank` %in% c('M4','M5'), "green", "red"))),
    'Revised_Rank'= formatter("span", style =
            ~ style(color = ifelse(Rank_Number > 3, "green", "red"))),
   "ResidentialCalc", "Residential_1a", "Residential_1b", "AgricultureCalc",  
   "Agriculture_2.1", "Agriculture_2.3b", "EnergyCalc", "Energy_3all",       
   "TransportationCalc", "Transport_4.1", "BioUseCalc", "BioUse_5.1a",       
   "BioUse_5.1b", "BioUse_5.3", "HumanIntrusionCalc", "HumanIntrusion_6",  
   "ClimateChangeCalc","ClimateChange_11" 
  ))
  
  #webshot::install_phantomjs()
  DT.list[[j]]<-DT
  export_formattable(DT,file.path(figsOutDir,paste("GB_ThreatTable.png",sep='')),width = "100%")
}


# Generate Table of summary of calc and expert results
ResTable <- RankMap1 %>%
  dplyr::select(POPULATION_NAME, Adults, PopIso, STATUS, 
                ExpertRank, ExpertOverallThreat, CalcRank, Threat_Class, Rank_Number)
colnames(ResTable) <- c('GBPU','Female Pop 2018','Popn Iso','2012 Status',
                        'Expert Rank','Expert Threat', 'Calculated Rank','Calculated Threat','Rank_Number')
#RankTable$GBPU <- factor(RankTable$GBPU)]

df.list<-list(ResTable)
DT.list<-list()
npages<-1
j<-1
for (j in 1:npages) {
  df<-df.list[[j]]
  rownames(df)<-NULL
  
  DT<-formattable(df[order(df$Rank_Number),], list(Rank_Number =FALSE,
      'GBPU'= formatter("span", style =
       ~ style(color = ifelse(Rank_Number > 3, "green", "red"))),
       # formattable::area(col = c('GBPU')) ~ color_tile = ifelse(RankNumber > 3, "green", "red"),
       formattable::area(col = c('Female Pop 2018')) ~ normalize_bar("pink", 0.1),
       'Popn Iso','Overall Threat',
       '2012 Status'= formatter("span", style =
       ~ style(color = ifelse(`2012 Status` %in% c('Viable'), "green", "red"))),
      'Expert Rank'= formatter("span", style =
       ~ style(color = ifelse(`Expert Rank` %in% c('M4','M4M5','M5'), "green", "red"))),
      'Expert Threat',
       'Calculated Rank'= formatter("span", style =
       ~ style(color = ifelse(Rank_Number > 3.5, "green", "red"))),
      'Calculated Threat'
  ))
  
  #webshot::install_phantomjs()
  DT.list[[j]]<-DT
  export_formattable(DT,file.path(figsOutDir,paste("GB_ThreatCalcExpertSummaryTable.png",sep='')))
}

#RankMap1$Rank_Number <- factor(RankMap1$Rank_Number)
sortRankMap<-RankMap1[order(RankMap1$Rank_Number),]

# Generate Table of summary of calc and expert results
df1 <- sortRankMap[1:28,] %>%
  dplyr::select(POPULATION_NAME, Adults, Trend, PopIso, STATUS, 
                Threat_Class, CalcRank, CalcSRank,Rank_Number)
colnames(df1) <- c('GBPU','Female Pop 2018','Trend','Popn Iso','2012 Status',
                   'Overall Threat','Compound Rank','Single Rank','Rank_Number')
#df1$GBPU <- factor(df1$GBPU)

df2 <- sortRankMap[29:55,] %>%
  dplyr::select(POPULATION_NAME, Adults, Trend, PopIso, STATUS, 
                Threat_Class, CalcRank, CalcSRank, Rank_Number)
colnames(df2) <- c('GBPU','Female Pop 2018','Trend','Popn Iso','2012 Status',
                   'Overall Threat','Compound Rank','Single Rank','Rank_Number')
#df2$GBPU <- factor(df2$GBPU)

df.list<-list(df1,df2)

DT.list<-list()
npages<-2
j<-1
for (j in 1:npages) {
  df<-df.list[[j]]
  rownames(df)<-NULL
  
  DT<-formattable(df[order(df$Rank_Number),], list(Rank_Number =FALSE,
      'GBPU'= formatter("span", style =
       ~ style(color = ifelse(Rank_Number > 3, "green", "red"))),
       formattable::area(col = c('Female Pop 2018')) ~ normalize_bar("pink", 0.1),
       'Trend','Popn Iso',
       '2012 Status'= formatter("span", style =
        ~ style(color = ifelse(`2012 Status` %in% c('Viable'), "green", "red"))),
       'Overall Threat'= formatter("span", style =
       ~ style(color = ifelse(`Overall Threat` %in% c('Negligible','Low'), "green", 
                              ifelse(`Overall Threat` %in% c('Medium'), "orange",
                              "red")))),
      'Compound Rank'= formatter("span", style =
                          ~ style(color = ifelse(`Compound Rank` %in% c('M4','M4M5','M5'), "green", 
                                                 ifelse(`Compound Rank` %in% c('M3','M3M4'), "orange",      
                                                        "red")))),
      'Single Rank'= formatter("span", style =
                          ~ style(color = ifelse(`Single Rank` %in% c('M4','M5'), "green", 
                                                 ifelse(`Single Rank` %in% c('M3'), "orange",      
                                                        "red"))))
  ))
  
  #webshot::install_phantomjs()
  DT.list[[j]]<-DT
  export_formattable(DT,file.path(figsOutDir,paste("GB_ThreatSummaryTable_",j,".png",sep='')))
}

