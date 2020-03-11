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

# Load Provincial Boundary, Land Form, BTM, GBPU, CE data, 
# Livestock Density, Human Density, GBPU

#Rasterize the Province for subsequent masking
ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                 ymn=173787.5, ymx=1748187.5, 
                 crs="+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",
                 res = c(100,100), vals = 0)

BCr_file <- file.path(dataOutDir,"BCr.tif")
if (!file.exists(BCr_file)) {
  BCr <- fasterize(bcmaps::bc_bound_hres(class='sf'),ProvRast)
  writeRaster(BCr, filename=BCr_file, format="GTiff", overwrite=TRUE)
} else {
  BCr <- raster(BCr_file)
}

#BTM - for settlement, rural, agriculture, range, mining
# read from Bear_Data repo
AgricultureR <- raster(file.path(GBspatialDir,"AgricultureR.tif"))
RangeR <- raster(file.path(GBspatialDir,"RangeR.tif"))
UrbanR <- raster(file.path(GBspatialDir,"UrbanR.tif"))
MiningR <- raster(file.path(GBspatialDir,"MiningR.tif"))
RecR <- raster(file.path(GBspatialDir,"RecR.tif"))

BTM_file <- file.path("tmp/BTM_Brick")
BTM_Brick<- brick(AgricultureR, RangeR, UrbanR, MiningR, RecR)
names(BTM_Brick) <- c('AgricultureR','RangeR','UrbanR','MiningR','RecR')
saveRDS(BTM_Brick, file = BTM_file)

# CE data
# GB Security Areas, Human Access and Road Density
# from GB_Data repo: https://github.com/bcgov/GB_Data
#Security Areas
SecureR<-raster(file.path(GBspatialDir,"SecureR.tif"))
#SecureZ<-zonal(SecureR,GBPUr_NonHab,'sum', na.rm=TRUE)


#Human access
FrontCountryR<-raster(file.path(GBspatialDir,"FrontCountryR.tif"))

#Road Density
RdDensR<-raster(file.path(GBRdDir,"RoadDensR.tif"))

#Salmon Change
SalmonChangeR<-raster(file.path(GBspatialDir,"SalmonChangeR.tif"))

#Mid Seral
MidSeralR<-raster(file.path(GBspatialDir,"MidSeralR.tif"))

# Hunter Density
# from HunterDensity repo: https://github.com/bcgov/HunterDensity
HuntDDensR<-raster(file.path(HunterSpatialDir,"HuntDDensR.tif"))
HuntDDensNonHabR<-raster(file.path(HunterSpatialDir,"HuntDDensNonHabR.tif"))

#Human & Livestock Denisty
# from HumanLivestock repo: https://github.com/bcgov/HumanLivestockDensity
HumanDensityR<-raster(file.path(HumanLivestockSpatialDir,"HumanDensityR.tif"))
LivestockDensityR<-raster(file.path(HumanLivestockSpatialDir,"LSDensityR.tif"))

#Mortlaity - see GB_unreported - BioUse_5.1a
# from GB_Mortality repo: https://github.com/bcgov/GB_Mortality
#FemaleUnk_Report_pop<- data.frame(read_xls(file.path(MortDataDir,paste('FemaleUnk_Report_pop.xls',sep=''))))
FemaleUnk_Report_pop<- data.frame(read_xls(file.path(MortDataDir,paste('MortalityThreat_WMU_to_GBPU.xls',sep=''))))

#Point layers for development
#Placer and coal tenures - https://catalogue.data.gov.bc.ca/dataset/mta-mineral-placer-and-coal-tenure-spatial-view
#Not currently used - tenures not nec active - assume if active mining then in major project list
PlacerCoalS <- st_read(file.path(DataDir,'LandDisturbance/MTA_ACQUIRED_TENURE_SVW/MTA_ACQ_TE_polygon.shp'))
PlacerCoalS <- PlacerCoalS[PlacerCoalS$TNRSBTPDSC %in% c('LEASE','LICENSE') ,]

# Major Projects downloaded from- https://catalogue.data.gov.bc.ca/dataset/major-projects-inventory-economic-points
MajProjS <- st_read(file.path(DataDir,'LandDisturbance/MPI_ECON_MAJOR_PROJECTS_POINT/MPI_ECON_point.shp'))
MajProjS <- MajProjS[MajProjS$PRJ_STATUS %in% c('Completed', 'Proposed', 'Construction started') ,]

#Pull points for mining, oil, gas and wind and hydro
OilGasP <- (as(MajProjS[MajProjS$CNST_STYPE == 'Oil & Gas' ,], 'Spatial'))@coords[,1:2]
MiningP <- (as(MajProjS[MajProjS$CNST_STYPE == 'Mining' ,], 'Spatial'))@coords[,1:2]
WindHydroP <- (as(MajProjS[MajProjS$CNST_STYPE == 'Utilities' ,], 'Spatial'))@coords[,1:2]

#Files for linear features
Linear_file <- file.path("tmp/Linear_Brick")
if (!file.exists(Linear_file)) {
  #Linear raster files imported from CE data set into GRASS and rasterized
  OilGasR<-as.integer(raster(file.path(DataDir,"LandDisturbance/OilGasR.tif"), background=0, na.rm=TRUE)>0)
  crs(OilGasR)<-crs(OilGasR)
  TransR<-as.integer(raster(file.path(DataDir,"LandDisturbance/TransR.tif"), background=0, na.rm=TRUE)>0)
  crs(TransR)<-crs(TransR)
  #Road density rasterized by repo: https://github.com/bcgov/roadless-areas-indicator
  #Only uses DRA
  #RdDensR<-as.integer(raster(file.path(DataDir,"RoadDensR.tif"), background=0, na.rm=TRUE)>0)
  #Use CE 2017 roads - includes DRA, FTEN, and RESULTS roads - being updated 2019
  
  
  crs(RdDensR)<-crs(ProvRast)
  # Write out individual layers
  writeRaster(RdDensR, filename=file.path(DataDir,"LandDisturbance/RdDensR.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(OilGasR, filename=file.path(DataDir,"LandDisturbance/OilGasR.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(TransR, filename=file.path(DataDir,"LandDisturbance/TransR.tif"), format="GTiff", overwrite=TRUE)
  
  if (!file.exists(file.path(DataDir,"RailR.tif"))) {
    #Rail downloaded from https://catalogue.data.gov.bc.ca/dataset/railway-track-line
    RailS<-st_read(file.path(DataDir,'LandDisturbance/GBA_RAILWAY_TRACKS_SP/RW_TRACK_line.shp'))
    R1 <- rasterize(st_zm(RailS[RailS$RLWY_TR_ID >0 ,],drop=TRUE),ProvRast, background=0, na.rm=TRUE)
    R1[R1[]>0]<-1
    RailR<-setValues(raster(R1), R1[])
    writeRaster(RailR, filename=file.path(DataDir,"LandDisturbance/RailR.tif"), format="GTiff", overwrite=TRUE)
  } else {
    RailR<-as.integer(raster(file.path(DataDir,"LandDisturbance/RailR.tif"), background=0, na.rm=TRUE)>0)
    crs(RailR)<-crs(RailR)
  }  
  
  #NE Seismic - from Provincial CE
  if (!file.exists(file.path(DataDir,"SeismicR.tif"))) {
    S1<-st_read(file.path(DataDir,'LandDisturbance/NE_Seismic/NE_Seismic.shp'))
    SeismicL<-st_cast(S1, "LINESTRING", do_split=TRUE)
    S2 <- rasterize(st_zm(SeismicL[SeismicL$Shape_Leng >0 ,],drop=TRUE),ProvRast, background=0, na.rm=TRUE)
    S2[S2[]>0]<-1
    SeismicR<-setValues(raster(S2), S2[])
    writeRaster(SeismicR, filename=file.path(DataDir,"LandDisturbance/SeismicR.tif"), format="GTiff", overwrite=TRUE)
    
  } else {
    SeismicR<-as.integer(raster(file.path(DataDir,"LandDisturbance/SeismicR.tif"), background=0, na.rm=TRUE)>0)
    crs(SeismicR)<-crs(SeismicR)
  }  
  
  #Make a raster brick of linear features - not using due to raster memory allocation bug
  #Linear_Brick <- brick(RdDensR, RailR, SeismicR, OilGasR, TransR)
  #names(BTM_Brick) <- c('RdDensR','RailR','OilGasR','TransR','SeismicR')
  Linear_Brick <- brick(RdDensR, RailR, OilGasR, TransR, SeismicR)
  names(Linear_Brick) <- c('RdDensR','RailR','OilGasR','TransR','SeismicR')
  saveRDS(Linear_Brick, file = Linear_file)
} else {
  Linear_Brick<-readRDS(file = Linear_file)
  RailR<-raster(file.path(DataDir,"LandDisturbance/RailR.tif"))
  RdDensR<-raster(file.path(DataDir,"LandDisturbance/RdDensR.tif"))
  TransR<-raster(file.path(DataDir,"LandDisturbance/TransR.tif"))
  SeismicR<-raster(file.path(DataDir,"LandDisturbance/SeismicR.tif"))
  OilGasR<-raster(file.path(DataDir,"LandDisturbance/OilGasR.tif"))
}  

#Load Strata
# from GB_Data repo: https://github.com/bcgov/GB_Data
NonHab<-raster(file.path(StrataDir,"NonHab.tif"))
GBPUr<-raster(file.path(StrataDir,"GBPUr.tif"))
WMUr<-raster(file.path(StrataDir,"WMUr.tif"))
WMUr_NonHab<-raster(file.path(StrataDir,"WMUr_NonHab.tif"))
GBPUr_NonHab<-raster(file.path(StrataDir,"GBPUr_NonHab.tif"))
GBPUr_BEI_1_2<-raster(file.path(StrataDir,"GBPUr_BEI_1_2.tif"))
GBPUr_BEI_1_5<-raster(file.path(StrataDir,"GBPUr_BEI_1_5.tif"))
GBPUr_LFormFlat<-raster(file.path(StrataDir,"GBPUr_LFormFlat.tif"))
GBPUr_LFormFlatFlat<-raster(file.path(StrataDir,"GBPUr_LFormFlatFlat.tif"))
GBPUr_Forest<-raster(file.path(StrataDir,"GBPUr_Forest.tif"))

    #######
  #Other possible layers - not currently used
  #GB CE summary data
  GB_gdb <- list.files(file.path(BearsCEDir), pattern = ".gdb", full.names = TRUE)[1]
  gb_list <- st_layers(GB_gdb)

  GB <- read_sf(GB_gdb, layer = "LU_SUMMARY_poly_v5_20160210")
  GB_lut <- data.frame(LANDSCAPE_UNIT_PROVID=GB$LANDSCAPE_UNIT_PROVID,LANDSCAPE_UNIT_NAME=GB$LANDSCAPE_UNIT_NAME)
  
  GBPU<-read_sf(GB_gdb, layer = "GBPU_BC_edits_v2_20150601")
  saveRDS(GBPU, file = 'tmp/GBPU')
  
  GBPU_LEH_gdb<-list.files(file.path('../GB_Data/data/Population/Bear_Density_2018'),pattern=".gdb",full.names=TRUE)[1]
  GBPU_LEH_list <-st_layers(GBPU_LEH_gdb)
  GBPU_LEH <- read_sf(GBPU_LEH_gdb, layer='GBPU_MU_LEH_2015_2018_bear_density_DRAFT')
  saveRDS(GBPU_LEH, file = 'tmp/GBPU_LEH')
  
  # Make a LU_summary raster
  GB_CEr <- fasterize(GB, ProvRast, field = 'LANDSCAPE_UNIT_PROVID')


  #Read in LU csv 
#LU_Summ_in <- data.frame(read.csv(header=TRUE, file=paste(DataDir, "/Bears/GBear_LU_Summary_scores_v5_20160823.csv", sep=""), sep=",", strip.white=TRUE, ))
#Read in assessor based GBPU ranks
#Ranking_in <- data.frame(read.csv(header=TRUE, file=paste(DataDir, "/ProvGBPUs_NatServeMPSimplified.csv", sep=""), sep=",", strip.white=TRUE, ))
#Ranking_inSept <- data.frame(read.csv(header=TRUE, file=paste(DataDir, "/NS_GBPU_RANKS_MP_NEW_OCT_2018.csv", sep=""), sep=",", strip.white=TRUE, ))

#Read in assessor based GBPU ranks and updated population from most current GBPU assessment spreadsheet
#GBPop<- data.frame(read_xlsx(path=file.path(DataDir,paste('GBPU_Rank_25_June2019.xlsx',sep='')),sheet='Population'))
GBPop<- data.frame(read_xls(path=file.path(GBdataOutDir,'GBPUpop.xls'))) %>%
  mutate(GBPU_Name=POPULATION_NAME) %>%
  mutate(PopnEst2018=pop2018)

Ranking_in <- data.frame(read_xlsx(path=file.path(DataDir,paste('GBPU_Rank_25_June2019.xlsx',sep='')),sheet='RankWorkSheet'))
Trend <- data.frame(read_xlsx(path=file.path(DataDir,paste('GBPU_Rank_25_June2019.xlsx',sep='')),sheet='Trend'))

Region_LUT <- data.frame(read_xlsx(path=file.path(DataDir,paste('GBPU_Rank_25_June2019.xlsx',sep='')),sheet='Population')) %>%
  dplyr::select(GBPU_Name,Region)
saveRDS(Region_LUT, file = 'tmp/Region_LUT')

#Read in calculated GBPU isolation tables
Isolation_list <- import_list(file.path(DataDir,"Isolation/IsolationCalcTables.xlsx"))

