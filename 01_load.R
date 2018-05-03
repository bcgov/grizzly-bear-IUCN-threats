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
BTM_file <- file.path("tmp/BTM_Brick")
if (!file.exists(BTM_file)) {
  # Link to BTM file download from BCDC:
  # https://catalogue.data.gov.bc.ca/dataset/baseline-thematic-mapping-present-land-use-version-1-spatial-layer
  #Dowload file manually and put *.zip in this script and place file in the data directory
  BTMZip <- 'BCGW_78757263_1520272242999_7572.zip'
  unzip(file.path(DataDir, BTMZip), exdir = file.path(DataDir, "BTM"))
  
  # List feature classes in the geodatabase
  BTM_gdb <- list.files(file.path(DataDir, "BTM"), pattern = ".gdb", full.names = TRUE)[1]
  fc_list <- st_layers(BTM_gdb)
  BTM <- read_sf(BTM_gdb, layer = "WHSE_BASEMAPPING_BTM_PRESENT_LAND_USE_V1_SVW")
  
  # Pull out the BTM layers and rasterize
  #BTM[1,] #look at file header
  NonHab <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Fresh Water','Outside B.C.','Salt Water', 'Glaciers and Snow') ,] %>% 
    fasterize(ProvRast, background=NA)
  
  AgricultureR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c("Agriculture",'Residential Agriculture Mixtures') ,] %>% 
    fasterize(ProvRast, background=0)
  
  RangeR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Range Lands') ,] %>% 
    fasterize(ProvRast, background=0)
  
  UrbanR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Urban') ,] %>% 
    fasterize(ProvRast, background=0)
  
  MiningR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Mining') ,] %>% 
    fasterize(ProvRast, background=0)
  
  RecR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Recreation Activities') ,] %>% 
    fasterize(ProvRast, background=0)

  # Write out the BTM layers as individual rasters
  writeRaster(AgricultureR, filename=file.path(DataDir,"LandDisturbance/AgricultureR.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(RangeR, filename=file.path(DataDir,"LandDisturbance/RangeR.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(UrbanR, filename=file.path(DataDir,"LandDisturbance/Urban.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(MiningR, filename=file.path(DataDir,"LandDisturbance/Mining.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(RecR, filename=file.path(DataDir,"LandDisturbance/Rec.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(NonHab, filename=file.path(DataDir,"NonHab.tif"), format="GTiff", overwrite=TRUE)
  
  #Make a raster brick of layers - not using due to raster memory allocation bug
  BTM_Brick<- brick(AgricultureR, UrbanR, MiningR, RecR)
  names(BTM_Brick) <- c('AgricultureR','UrbanR','MiningR','RecR')
  saveRDS(BTM_Brick, file = BTM_file)
  
 } else {
   BTM_Brick <- readRDS(file = BTM_file)
   NonHab<-raster(file.path(DataDir,"NonHab.tif"))
 }  

#Point layers for development
#Placer and coal tenures - https://catalogue.data.gov.bc.ca/dataset/mta-mineral-placer-and-coal-tenure-spatial-view
#Not currently used
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
  SeismicR<-as.integer(raster(file.path(DataDir,"SeismicR.tif"), background=0, na.rm=TRUE)>0)
  crs(SeismicR)<-crs(ProvRast)
  #Road density rasterized by repo: https://github.com/bcgov/roadless-areas-indicator
  RdDensR<-as.integer(raster(file.path(DataDir,"RoadDensR.tif"), background=0, na.rm=TRUE)>0)
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
Linear_Brick <- brick(RdDensR, RailR, OilGasR, TransR)
names(BTM_Brick) <- c('RdDensR','RailR','OilGasR','TransR')
saveRDS(Linear_Brick, file = Linear_file)
} else {
  Linear_Brick<-readRDS(file = Linear_file)
  RailR<-raster(file.path(DataDir,"LandDisturbance/RailR.tif"))
  RdDensR<-raster(file.path(DataDir,"LandDisturbance/RdDensR.tif"))
  TransR<-raster(file.path(DataDir,"LandDisturbance/TransR.tif"))
  SeismicR<-raster(file.path(DataDir,"LandDisturbance/SeismicR.tif"))
  OilGasR<-raster(file.path(DataDir,"LandDisturbance/OilGasR.tif"))
}  

# Provincial CE Data
# List feature classes in the geodatabase and read in 
GB_file <- file.path("tmp/GB_Brick")
if (!file.exists(GB_file)) {
  GB_gdb <- list.files(file.path(DataDir, "Bears"), pattern = ".gdb", full.names = TRUE)[1]
  fc_list <- st_layers(GB_gdb)
  
  #Mortality
  Mort <- read_sf(GB_gdb, layer = "COMBINED_Grizzly_PopMort_Allocation_2004_to_2014")
  # Make a Mortality raster - 1-5 fails - what's the spatial - has had a past mort failure?
  Mortr <- Mort[Mort$Pop_Mort_TOTAL_AllocationP_Count_v1_allAreas >0 ,] %>% 
    fasterize(ProvRast, background=0)
  
  #Front Country
  FrontCountry <- read_sf(GB_gdb, layer = "FrontCountry_v2_Coastal_DC")
  # Make a Front Country raster - 3,4,5 area front - 1,2 are back - need to spatialize
  FrontCountryr <- FrontCountry[FrontCountry$Encounter_Class_Num %in% c(3,4,5) ,] %>% 
    fasterize(ProvRast, background=0)
  
  #Mid Seral
  MidSeral <- read_sf(GB_gdb, layer = "LU_midSeral_conifer")
  # Make a Mid Seral raster - 1 is low
  MidSeralr <- MidSeral[MidSeral$mid_Seral_Num == 1 ,] %>% 
    fasterize(ProvRast, background=0)
  
  # Hunter Day density per km2 LU_hunterDays_annual_per_km2
  HunterDayD <- read_sf(GB_gdb, layer = "LU_SUMMARY_poly_v5_20160210")
  # Make a Hunter Day density raster
  HunterDayDr <- HunterDayD %>% 
    fasterize(ProvRast, field='LU_hunterDays_annual_per_km2', background=0)
  
  #Cow density
  CDR1<-raster(file.path(DataDir,"LandDisturbance/CowDensityR"))
  CowDensityR<-setValues(raster(CDR1), CDR1[])
  
#Write rasters
  writeRaster(MidSeralr, filename=file.path(DataDir,"LandDisturbance/MidSeralr.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(FrontCountryr, filename=file.path(DataDir,"LandDisturbance/FrontCountryr.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(HunterDayDr, filename=file.path(DataDir,"LandDisturbance/HunterDayDr.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(Mortr, filename=file.path(DataDir,"LandDisturbance/Mortr.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(CowDensityR, filename=file.path(DataDir,"LandDisturbance/CowDensityR.tif"), format="GTiff", overwrite=TRUE)
  
  #Make a raster stack of GB rasters and save to disk - not using due to raster memory allocation bug
  GB_Brick<-brick(Mortr, FrontCountryr, MidSeralr) 
  names(GB_Brick) <- c('Mortr','FrontCountryr','MidSeralr')
  saveRDS(GB_Brick, file = GB_file)
  
  #Organize strata layers
  #BEI
  BEI <- read_sf(GB_gdb, layer = "Final_Grizzly_BEI")
  # Make a BEI raster
  BEI_1_2_r <- BEI[BEI$HIGHCAP %in% c(1,2) ,] %>% 
    fasterize(ProvRast, background=NA)
  BEI_1_5_r <- BEI[BEI$HIGHCAP %in% c(1,2,3,4,5) ,] %>% 
    fasterize(ProvRast, background=NA)
  BEIr <- fasterize(BEI, ProvRast, background=0, field='HIGHCAP')
  #GBPU
  GBPU <- read_sf(GB_gdb, layer = "GBPU_BC_edits_v2_20150601")
  GBPU_lut <- tidyr::replace_na(data.frame(GRIZZLY_BEAR_POP_UNIT_ID=GBPU$GRIZZLY_BEAR_POP_UNIT_ID, POPULATION_NAME=GBPU$POPULATION_NAME, stringsAsFactors = FALSE), list(POPULATION_NAME = 'extirpated'))
  saveRDS(GBPU, file = 'tmp/GBPU')
  saveRDS(GBPU_lut, file = 'tmp/GBPU_lut')
  
  # Make a GBPU raster
  GBPUr <- fasterize(GBPU, ProvRast, field = 'GRIZZLY_BEAR_POP_UNIT_ID', background=NA)
  writeRaster(GBPUr, filename=file.path(DataDir,"Strata/GBPUr.tif"), format="GTiff", overwrite=TRUE)
  #set GBPUr to NA where BEI is NonHab
  GBPUr_NonHab <- overlay(GBPUr, NonHab, fun = function(x, y) {
    x[!is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_NonHab, filename=file.path(DataDir,"Strata/GBPUr_NonHab.tif"), format="GTiff", overwrite=TRUE)
  
  GBPUr_BEI_1_2 <- overlay(GBPUr_NonHab, BEI_1_2_r, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_BEI_1_2, filename=file.path(DataDir,"Strata/GBPUr_BEI_1_2.tif"), format="GTiff", overwrite=TRUE)
  
  GBPUr_BEI_1_5 <- overlay(GBPUr_NonHab, BEI_1_5_r, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_BEI_1_5, filename=file.path(DataDir,"Strata/GBPUr_BEI_1_5.tif"), format="GTiff", overwrite=TRUE)
  
  #Landform from AdaptWest
  # Need to pull strata of interest - valley bottom
  LForm_file <- file.path(dataOutDir,"LForm.tif")
  LFormFlat_file <- file.path(dataOutDir,"LFormFlat.tif")
  if (!file.exists(LForm_file)) {
    LForm<-mask(raster(file.path(DataDir,"Landform_BCAlbs.tif")) %>%
           resample(ProvRast, method='ngb'), BCr)

    LF_lut<-read_csv(file.path(DataDir,'landform_lut.csv'), col_names=TRUE)
    writeRaster(LForm, filename=file.path(DataDir,"LForm.tif"), format="GTiff", overwrite=TRUE)
    saveRDS(LF_lut, file = 'tmp/LF_lut')
 
    # Pull out just the flat areas - valley bottom (1000) and plains (5000)
    LFormFlat<-LForm
    LFormFlat[!(LFormFlat[] %in% c(1000,5000,6000,7000,8000))]<-NA
    writeRaster(LFormFlat, filename=file.path(DataDir,"Strata/LFormFlat.tif"), format="GTiff", overwrite=TRUE)
    # Pull out really flat areas - valley bottom (1000) and plains (5000)
    LFormFlatFlat<-LForm
    LFormFlatFlat[!(LFormFlatFlat[] %in% c(1000,5000))]<-NA
    writeRaster(LFormFlatFlat, filename=file.path(DataDir,"Strata/LFormFlatFlat.tif"), format="GTiff", overwrite=TRUE)
  } else {
    LForm <- raster(file.path(DataDir,"LForm.tif"))
    LFormFlat <- raster(file.path(DataDir,"Strata/LFormFlat.tif"))
    LFormFlatFlat <- raster(file.path(DataDir,"Strata/LFormFlatFlat.tif"))
    LF_lut<- readRDS(file = 'tmp/LF_lut')
  }
  
  GBPUr_LFormFlat <- overlay(GBPUr_NonHab, LFormFlat, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_LFormFlat, filename=file.path(DataDir,"Strata/GBPUr_LFormFlat.tif"), format="GTiff", overwrite=TRUE)
  
  GBPUr_LFormFlatFlat <- overlay(GBPUr_NonHab, LFormFlatFlat, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_LFormFlatFlat, filename=file.path(DataDir,"Strata/GBPUr_LFormFlatFlat.tif"), format="GTiff", overwrite=TRUE)
  
  #Read in landcover
  LandCover<-raster(file.path(DataDir,"LandCover/land_cover_n_age_2017.tif"))
  LC_lut<-read_csv(file.path(DataDir,'LandCover/LandCover_lut.csv'), col_names=TRUE)
  Forest<-LandCover
  Forest[!(Forest[] > 0)]<-NA
  GBPUr_Forest <- overlay(GBPUr_NonHab, Forest, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_Forest, filename=file.path(DataDir,"Strata/GBPUr_Forest.tif"), format="GTiff", overwrite=TRUE)
  
    #######
  #Other possible layers - not currently used
  #GB CE summary data
  GB <- read_sf(GB_gdb, layer = "LU_SUMMARY_poly_v5_20160210")
  GB_lut <- data.frame(LANDSCAPE_UNIT_PROVID=GB$LANDSCAPE_UNIT_PROVID,LANDSCAPE_UNIT_NAME=GB$LANDSCAPE_UNIT_NAME)
  
  # Make a LU_summary raster
  GB_CEr <- fasterize(GB, ProvRast, field = 'LANDSCAPE_UNIT_PROVID')
  
} else {
  NonHab<-raster(file.path(DataDir,"NonHab.tif"))
  GBPUr<-raster(file.path(DataDir,"Strata/GBPUr.tif"))
  GBPUr_NonHab<-raster(file.path(DataDir,"Strata/GBPUr_NonHab.tif"))
  GBPUr_BEI_1_2<-raster(file.path(DataDir,"Strata/GBPUr_BEI_1_2.tif"))
  GBPUr_BEI_1_5<-raster(file.path(DataDir,"Strata/GBPUr_BEI_1_5.tif"))
  GBPUr_LFormFlat<-raster(file.path(DataDir,"Strata/GBPUr_LFormFlat.tif"))
  GBPUr_LFormFlatFlat<-raster(file.path(DataDir,"Strata/GBPUr_LFormFlatFlat.tif"))
  GBPUr_Forest<-raster(file.path(DataDir,"Strata/GBPUr_Forest.tif"))
  GB_Brick <- readRDS(file = GB_file)
}  

#Read in LU csv 
LU_Summ_in <- data.frame(read.csv(header=TRUE, file=paste(DataDir, "/Bears/GBear_LU_Summary_scores_v5_20160823.csv", sep=""), sep=",", strip.white=TRUE, ))
#Read in assessor based GBPU ranks
Ranking_in <- data.frame(read.csv(header=TRUE, file=paste(DataDir, "/ProvGBPUs_NatServeMPSimplified.csv", sep=""), sep=",", strip.white=TRUE, ))
#Read in calculated GBPU isolation tables
Isolation_list <- import_list(file.path(DataDir,"Isolation/IsolationCalcTables.xlsx"))

