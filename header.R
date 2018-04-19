library(sf)
library(dplyr)
library(plyr)
library(readr)
library(raster)
library(bcmaps)
library(fasterize)
library(tidyr)

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
StrataOutDir <- file.path(dataOutDir,'Strata')
tileOutDir <- file.path(dataOutDir,'tile')
figsOutDir <- file.path(OutDir,'figures')
DataDir <- 'data'
dir.create(file.path(OutDir), showWarnings = FALSE)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(StrataOutDir), showWarnings = FALSE)
dir.create(file.path(tileOutDir), showWarnings = FALSE)
dir.create(file.path(figsOutDir), showWarnings = FALSE)
dir.create(DataDir, showWarnings = FALSE)
dir.create("tmp", showWarnings = FALSE)



