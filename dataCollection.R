
# ---------------- 0.1. SET UP - Packages and WD ---------------- 

## Data import and API keys
#
# install.packages("ZillowR")
library(ZillowR) # Cite
# install.packages("assertthat")
# library(assertthat)
# install.packages("devtools")
# library(devtools)
# devtools::install_github("xiyuansun/realEstAnalytics")
library(realEstAnalytics) # Cite
set_zillow_web_service_id('X1-ZWz17e71z5vswb_3mmvk')
#
# install.packages("censusapi")
library(censusapi)
Sys.setenv(CENSUS_KEY="bef9711de6b9166f981c672e3bd7d0c9569e9a89")
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")
#
# install.packages("rnoaa")
library(rnoaa) # Cite
# usethis::edit_r_environ()
Sys.setenv(NOAA_KEY = "rGBcYgZXrRSAvjncsfFEQQLXyYnjdIYI")
#
# install.packages("osmdata")
library(osmdata)
# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html

## Data analysis and visualization
library(ggplot2)
library(RColorBrewer)
# install.packages("dplyr")
library(dplyr)
# install.packages("stringr")
library(stringr)

## Geospatial Analysis
# install.packages("RCurl")
library(RCurl) # Cite
# install.packages("raster")
library(raster) # Cite
# install.packages("rgdal")
library(rgdal) # Cite
# install.packages("rgeos")
library(rgeos) # Cite
# install.packages("sf")
library(sf)
sf_use_s2(FALSE) # To avoid intersection error
# install.packages("GGally")
library(GGally)

## Analyses
# install.packages("ecodist")
library(ecodist)
# install.packages("oaxaca")
library(oaxaca)
# install.packages("nlme")
require(nlme)
# install.packages("lm4")
library(lme4)
# install.packages("lmerTest")
library(lmerTest)
# install.packages("MuMIn")
require(MuMIn)
# install.packages("sjPlot")
library(sjPlot)
# install.packages("sjmisc")
library(sjmisc)
# install.packages("cluster")
library(cluster)
# install.packages("vegan")
library(vegan)
# install.packages("zoo")
library(zoo)

## Bayesian Analysis
# install.packages("rstan", source = TRUE)
library(rstan)
# install.packages("rstanarm", source = TRUE)
library(rstanarm)
# install.packages("rstantools", source = TRUE)
library(rstantools)


RootWD <- gsub("Scripts", "Scratch", getwd())
states <- c("ME", "VT", "NH", "MA", "RI", "CT", 
            "NY", "NJ", "DE", "MD", "PA", 
            "VA", "NC", "SC", "GA", "FL", 
            "AL", "MS", "LA", "TX")

# ---------------- 0.2. SET UP - Functions ---------------- 

naCount <- function(list) {return(length(which(is.na(list))))}
zeroCount <- function(list) {return(length(which(list==0)))}
summaryInfo <- function(object) {
  return(c(row.names(summary(object)$coefficients)[which.max(summary(object)$coefficients[127:(nrow(summary(object)$coefficients)-433),4])+126],
           summary(object)$coefficients[which.max(summary(object)$coefficients[127:(nrow(summary(object)$coefficients)-433),4])+126,4],
           summary(object)$r.squared,
           summary(object)$adj.r.squared))
}
corr2m <- function(df1, df2) {
  corrMax <- as.data.frame(matrix(ncol = ncol(df1),
                                  nrow = ncol(df2)))
  for(col1 in 1:ncol(df1)){
    for (col2 in 1:ncol(df2)){
      corrMax[col2, col1] <- as.numeric(cor.test(df2[,col2], df1[,col1])$estimate)
    }
  }
  row.names(corrMax) <- names(df2)
  names(corrMax) <- names(df1)
  return(corrMax)
}

reldata <- function(x, byrow = TRUE, bycol = FALSE, rowfirst=TRUE)
{
  # Relativize x by row sum, column maximum or both.
  # See also scale().
  # offers option to process row or column first
  # wisconsin standardization: rel(x, byrow = TRUE, bycol=TRUE, rowfirst=FALSE)
  # SCG 2012-02-21
  # CREDIT: DEAN URBAN
  
  if(rowfirst) {
    if(byrow) {
      rsum <- apply(x, 1, sum)
      rsum[rsum == 0] <- 1
      x <- sweep(x, 1, rsum, "/")
    }
    if(bycol) {
      cmax <- apply(x, 2, max)
      cmax[cmax == 0] <- 1
      x <- sweep(x, 2, cmax, "/")
    }
  } else {
    if(bycol) {
      cmax <- apply(x, 2, max)
      cmax[cmax == 0] <- 1
      x <- sweep(x, 2, cmax, "/")
    }
    if(byrow) {
      rsum <- apply(x, 1, sum)
      rsum[rsum == 0] <- 1
      x <- sweep(x, 1, rsum, "/")
    }
  }
  x
}

EnterTCoeff <- function(T_DF, Model) {
  ModelCoeff <- summary(Model)$coefficients
  T_DF$Entry <- rep(NA, nrow(T_DF))
  for (row in 1:nrow(T_DF)) {
    if (row.names(T_DF)[row] %in% row.names(ModelCoeff)) {
      Row_ModCf <- which(row.names(ModelCoeff) == row.names(T_DF)[row])
      T_DF$Entry[row] <- ModelCoeff[Row_ModCf,4]
    }
  }
  names(T_DF)[ncol(T_DF)] <- toString(quote(Model))
  return(T_DF)
}

EnterPCoeff <- function(P_DF, Model) {
  ModelCoeff <- summary(Model)$coefficients
  P_DF$Entry <- rep(NA, nrow(P_DF))
  for (row in 1:nrow(P_DF)) {
    if (row.names(P_DF)[row] %in% row.names(ModelCoeff)) {
      Row_ModCf <- which(row.names(ModelCoeff) == row.names(P_DF)[row])
      P_DF$Entry[row] <- ModelCoeff[Row_ModCf,5]
    }
  }
  names(P_DF)[ncol(P_DF)] <- toString(quote(Model))
  return(P_DF)
}

# ---------------- 1.1. IMPORT - Storm Data ---------------- 

# ZIP info file
# Make a list of all ZIP units in the study area
setwd(RootWD)
ZipInfo <- read.csv("ZipInfo.csv") %>%
  filter(ST %in% states)
#
zip_ls = c()
for (i in 1:nrow(ZipInfo)) {
  zip_ls = c(zip_ls, ZipInfo$Zip.Min[i]:ZipInfo$Zip.Max[i])
}
zip_ls = as.character(zip_ls[!duplicated(zip_ls)])
zip_ls = str_pad(zip_ls, 5, pad="0")

# ZIP Boundary Shapefile
ZipBdry <- st_read("ZipBdry.shp") # Package: sf
ZipBdry = ZipBdry[ZipBdry$ZCTA5CE10 %in% zip_ls,]
StormData <- data.frame(data.frame(ZipBdry)[,1])
names(StormData) <- "ZIP"
# https://data-nconemap.opendata.arcgis.com/datasets/zip-code-tabulation-areas
# Cite


setwd(paste(RootWD, "/StormWindSwath", sep = ""))
for (year in 2008:2020){
  for (number in 1:30){
    if (number < 10){
      number <- paste("0", number, sep="")
    }
    # Define location and filenames
    storm_url <- paste("https://www.nhc.noaa.gov/gis/best_track/al", 
                       number, year, "_best_track.zip",
                       sep="")
    storm_loc <- paste("/windswath",
                       number, year, ".zip",
                       sep = "")
    storm_shp <- paste("al", number, year, "_windswath.shp",
                       sep = "")
    if (url.exists(storm_url)){
      # Download and unzip Hurricane Data
      download.file(storm_url, storm_loc)
      unzip(storm_loc)
      if (!file.exists(storm_shp)){
        print(paste(number, year, "does not seem to exist!"))
        next
      }
      storm_shp <- st_read(storm_shp) # Package: sf
      storm_shp <- st_transform(storm_shp, st_crs(ZipBdry)) # Package: sf
      # Geoprocessing 
      storm_itrs <- st_intersection(ZipBdry, storm_shp) # Package: sf
      storm_itrs <- data.frame(storm_itrs)
      StormData$NewCol <- rep(NA, nrow(StormData))
      for (zip in unique(storm_itrs$GEOID10)){
        Entry <- max(storm_itrs[which(storm_itrs$GEOID10 == zip),]$RADII)
        StormData[which(StormData$ZIP == zip),]$NewCol <- Entry
      }
      names(StormData) <- append(names(StormData)[1:length(StormData)-1], 
                                 paste(number, year, sep = ""))
    }
  }
}

# Clean the data a little bit
val_count <- sapply(StormData, function(y) sum(length(which(!is.na(y)))))
StormData = StormData[,which(val_count != 0)]

# Save Storm Data output
setwd(gsub("Scratch", "Output", RootWD))
write.csv(StormData, "0101_StormData_230306.csv")
setwd(RootWD)

# Matrix of hurricane time
setwd(paste(RootWD, "/StormWindSwath", sep = ""))
StormTime <- data.frame(matrix(ncol = 3, nrow = 56))
n = 0
for (year in 2008:2020){
  for (number in 1:30){
    if (number < 10){
      number <- paste("0", number, sep="")
    }
    storm_shp <- paste("al", number, year, "_windswath.shp",
                       sep = "")
    name = paste("X", number, year, sep = "") 
    # If processing directly from StormData (not save and import)
    # Remove "X", 
    if (name %in% names(StormData)[2:ncol(StormData)]){
      storm_shp <- st_read(storm_shp) # Package: sf
      storm_shp <- data.frame(storm_shp)
      entry <- c(name, 
                 min(as.numeric(as.character(storm_shp$STARTDTG))),
                 max(as.numeric(as.character(storm_shp$ENDDTG))))
      n = n + 1
      StormTime[n,] <- entry
    }
  }
} # missing: 012009, 082016, 102017
names(StormTime) <- c("Event", "Start", "End")

# Save Storm Time output
setwd(gsub("Scratch", "Output", RootWD))
write.csv(StormTime, "0101_StormTime_230306.csv")
setwd(RootWD)



# ---------------- 1.2.1. IMPORT - Zillow Data from API ---------------- 

# Load files from before
setwd(gsub("Scratch", "Output", RootWD))
StormData <- read.csv("01_StormData_230306.csv", sep = ",")
StormTime <- read.csv("02_StormTime_230306.csv", sep = ",")
setwd(RootWD)

# Load All ZHVI data 
ZipAllMed <- get_ZHVI_series(bedrooms = 1, geography = "Zip", allhomes = T,
                             tier = "ALL", summary = F, other = NULL)
# https://rdrr.io/github/xiyuansun/realEstAnalytics/man/get_ZHVI_series.html
# Cite
ZipAllMed <- ZipAllMed[which(ZipAllMed$State == "NC"),]

# Load Bottom Tier ZHVI data
AllBot <- get_ZHVI_series(bedrooms = 1, geography = "Zip", allhomes = T,
                             tier = "B", summary = F, other = NULL)
AllBot <- AllBot[which(AllBot$State == "NC"),]

# Load SFR ZHVI
setwd(paste(RootWD, "/Zillow", sep = ""))
SFR_ZHVI <- read.csv("ZHVI_SFR.csv", sep = ",")
SFR_ZHVI <- SFR_ZHVI[which(ZipAllMed$State == "NC"),]
names(SFR_ZHVI) <- names(ZipAllMed)

# Load All ZRI (Rental)
setwd(paste(RootWD, "/Zillow", sep = ""))
AllZRI <- read.csv("ZRI_All.csv", sep = ",")
AllZRI <- AllZRI[which(AllZRI$State == "NC"),]
names(AllZRI)[8:119] <- names(ZipAllMed)[186:297]


# ---------------- 1.2.2. IMPORT - Zillow Data from direct web download ---------------- 

# Load ZHVI data 
# https://www.zillow.com/research/data/
ZHVI_All <- read.csv("ZHVI_All.csv")
ZHVI_All = ZHVI_All[ZHVI_All$State %in% states,]
names(ZHVI_All)[1] <- "ZIP"

# Load ZRI data
ZRI_All <- read.csv("ZRI_All.csv")
ZRI_All = ZRI_All[ZRI_All$State %in% states,]
names(ZRI_All)[1] <- "ZIP"

# Save them
setwd(gsub("Scratch", "Output", RootWD))
write.table(ZHVI_All, "0102_ZHVI_230306.csv", row.names = FALSE, sep = ",")
write.table(ZRI_All, "0102_ZRI_230306.csv", row.names = FALSE, sep = ",")
setwd(RootWD)


# ---------------- 1.3.1. IMPORT - Census Data, Multi-years, NC  ---------------- 


test <- getCensus(name = "acs/acs5",
                  vintage = 2017, vars = "NAME",
                  key = "bef9711de6b9166f981c672e3bd7d0c9569e9a89",
                  "zip+code+tabulation+area:*&in=state:*")
acs_group <- getCensus(name = "acs/acs5",
                       vintage = 2017,
                       vars = "group(B01003)",
                       region = "zip code tabulation area:*")
acs_group <- acs_group[which(as.numeric(acs_group$zip_code_tabulation_area) %in% 
as.numeric(as.character(CensusData$ZIP))),]
head(acs_group)

save_loc = paste(RootWD, "/Census2014-2017", sep = "")
var_ls = c("S0101", "S1201", "S1501", "S2301", "S2401", "S2501", "B01003", 
           "B02001", "B07001", "B08008", "B08013", "B08201","B08202", "B08301", 
           "B09001", "B11011", "B11016", "B12001", "B14001", "B14003", "B17001", 
           "B19013", "B19083", "B19113", "B19301", "B25035", "B25039", "B25041", 
           "B25064", "B25077", "B25081", "BP_2016_00CZ1")
for (var in var_ls) {
  acs_group <- getCensus(name = "acs/acs5",
                         vintage = 2017,
                         vars = paste0("group(",var,")"),
                         region = "zip code tabulation area:*")
}

c("B01003", "B09001")
c("B01003_001E", "B09001_001E")

Year = '14'
# S0101, S1201, S1501, S2301, S2401, S2501, B01003, B02001, B07001, B08008, B08013, B08201, 
# B08202, B08301, B09001, B11011, B11016, B12001, B14001, B14003, B17001, B19013, 
# B19083, B19113, B19301, B25035, B25039, B25041, B25064, B25077, B25081, BP_2016_00CZ1

# Independent Variables (Census 2013-2017):
setwd(paste(RootWD, "/Census2017", sep = ""))
# - Total population                          B01003 - HD01_VD01
# - logarithm
TotPop <- read.csv(sprintf("ACS_%s_5YR_B01003_with_ann.csv", Year), header = T)[-1,c(2,4)]
names(TotPop) <- c("ZIP", "Value")
CensusData <- TotPop
names(CensusData) <- c("ZIP", "PopTot")
CensusData$PopTot <- as.numeric(as.character(CensusData$PopTot))
# - Population under 18                       B09001 - HD01_VD01
TotChild <- read.csv(sprintf("ACS_%s_5YR_B09001_with_ann.csv", Year), header = T)[-1,c(2,4)]
names(TotChild) <- c("ZIP", "Value")
CensusData$PopPerChild <- as.numeric(as.character(TotChild$Value))/CensusData$PopTot

PopChar <- read.csv(sprintf("ACS_%s_5YR_S0101_with_ann.csv", Year), header = T)[-1,]
TotPopChar <- as.numeric(as.character(PopChar[,which(names(PopChar) == "HC01_EST_VC01")]))
# - Population over 65                        S0101 - 
### - 2017: HC02_EST_VC33
### - 2014-2016: HC01_EST_VC31
TotOld <- PopChar[,c(2, which(names(PopChar) == "HC01_EST_VC31"))] 
names(TotOld) <- c("ZIP", "Value") 
PerOld <- TotOld
PerOld$Value <- as.numeric(as.character(PerOld$Value))/100
CensusData$PopPerOld <- PerOld$Value

# - Median Age                                S0101 - 
### - 2017: HC01_EST_VC37
### - 2014-2016: HC01_EST_VC35
MedAge <- PopChar[,c(2, which(names(PopChar) == "HC01_EST_VC35"))]
names(MedAge) <- c("ZIP", "Value")
CensusData$PopMedAge <- as.numeric(as.character(MedAge$Value))
# - Sex Percentage                            S0101 - 
### - 2017: HC03_EST_VC01/HC05_EST_VC01
### - 2015-2016: HC02_EST_VC01/HC03_EST_VC01
TotMale <- PopChar[,c(2, which(names(PopChar) == "HC02_EST_VC01"))]
names(TotMale) <- c("ZIP", "Value") 
PerMale <- TotMale
PerMale$Value <- as.numeric(as.character(PerMale$Value))/TotPopChar
CensusData$PopPerMale <- PerMale$Value
TotFemale <- PopChar[,c(2, which(names(PopChar) == "HC03_EST_VC01"))] 
names(TotFemale) <- c("ZIP", "Value") 
PerFemale <- TotFemale
PerFemale$Value <- as.numeric(as.character(PerFemale$Value))/TotPopChar
CensusData$PopPerFemale <- PerFemale$Value

PopRace <- read.csv(sprintf("ACS_%s_5YR_B02001_with_ann.csv", Year), header = T)[-1,]
TotPopRace <- as.numeric(as.character(PopRace[,which(names(PopRace) == "HD01_VD01")]))
# - Percent black & hispanic                  B02001 - HD01_VD03
TotBlk <- PopRace[,c(2, which(names(PopRace) == "HD01_VD03"))]
names(TotBlk) <- c("ZIP", "Value") 
PerBlk <- TotBlk
PerBlk$Value <- as.numeric(as.character(PerBlk$Value))/TotPopChar
CensusData$PopPerBlk <- PerBlk$Value
# - percent Asian                             B02001 - HD01_VD05
TotAsian <- PopRace[,c(2, which(names(PopRace) == "HD01_VD05"))]
names(TotAsian) <- c("ZIP", "Value") 
PerAsian <- TotAsian
PerAsian$Value <- as.numeric(as.character(PerAsian$Value))/TotPopChar
CensusData$PopPerAsian <- PerAsian$Value
# - percent white                             B02001 - HD01_VD02
TotWhite <- PopRace[,c(2, which(names(PopRace) == "HD01_VD02"))]
names(TotWhite) <- c("ZIP", "Value") 
PerWhite <- TotWhite
PerWhite$Value <- as.numeric(as.character(PerWhite$Value))/TotPopChar
CensusData$PopPerWhite <- PerWhite$Value

PopMari <- read.csv(sprintf("ACS_%s_5YR_S1201_with_ann.csv", Year), header = T)[-1,]
# - percent Married                          S1201 - HC02_EST_VC01
PerMarried <- PopMari[,c(2, which(names(PopMari) == "HC02_EST_VC01"))]
names(PerMarried) <- c("ZIP", "Value") 
PerMarried$Value <- as.numeric(as.character(PerMarried$Value))/100
CensusData$PopPerMarried <- PerMarried$Value
# - percent Divorced                        S1201 - HC04_EST_VC01
PerDiv <- PopMari[,c(2, which(names(PopMari) == "HC04_EST_VC01"))]
names(PerDiv) <- c("ZIP", "Value") 
PerDiv$Value <- as.numeric(as.character(PerDiv$Value))/100
CensusData$PopPerDiv <- PerDiv$Value
# - percent Never Married                    S1201 - HC06_EST_VC01
PerNever <- PopMari[,c(2, which(names(PopMari) == "HC06_EST_VC01"))]
names(PerNever) <- c("ZIP", "Value") 
PerNever$Value <- as.numeric(as.character(PerNever$Value))/100
CensusData$PopPerNever <- PerNever$Value



HouseOcc <- read.csv(sprintf("ACS_%s_5YR_S2501_with_ann.csv", Year), header = T)[-1,]
TotHouseOcc <- as.numeric(as.character(HouseOcc[,which(names(HouseOcc) == "HC01_EST_VC01")]))
# - percent owner-occupied                    S2501 -
### - 2017: HC03_EST_VC01
### - 2014-2016: HC02_EST_VC01
TotHouseOwner <- HouseOcc[,c(2, which(names(HouseOcc) == "HC02_EST_VC01"))]
names(TotHouseOwner) <- c("ZIP", "Value") 
PerHouseOwner <- TotHouseOwner
PerHouseOwner$Value <- as.numeric(as.character(PerHouseOwner$Value))/TotHouseOcc
CensusData$HousePerOwner <- PerHouseOwner$Value
# - percent renter-occupied                   S2501 - 
### - 2017: HC05_EST_VC01
### - 2014-2016: HC03_EST_VC01
TotHouseRent <- HouseOcc[,c(2, which(names(HouseOcc) == "HC03_EST_VC01"))]
names(TotHouseRent) <- c("ZIP", "Value") 
PerHouseRent <- TotHouseRent
PerHouseRent$Value <- as.numeric(as.character(PerHouseRent$Value))/TotHouseOcc
CensusData$HousePerRent <- PerHouseRent$Value
# - Household Type by size (Average)          S2501
persons <- c(1,2,3,4)
'
### - 2017:
cols <- c(16,28,40,52)
cols_own <- cols+4
cols_rent <- cols+8
'
### - 2015-2016:
cols <- c(10,16,22,28)
cols_own <- cols+2
cols_rent <- cols+2
pop <- rep(0, nrow(HouseOcc))
pop_own <- rep(0, nrow(HouseOcc))
pop_rent <- rep(0, nrow(HouseOcc))
for (i in 1:length(persons)){
  pop <- pop + persons[i] * as.numeric(as.character(HouseOcc[,cols[i]]))
  pop_own <- pop_own + persons[i] * as.numeric(as.character(HouseOcc[,cols_own[i]]))
  pop_rent <- pop_rent + persons[i] * as.numeric(as.character(HouseOcc[,cols_rent[i]]))
}
AveHouseSize <- data.frame(ZIP = HouseOcc[,2], # S2501
                           value = pop/TotHouseOcc)
AveHouseOwnSize <- data.frame(ZIP = HouseOcc[,2], # S2501
                              value = pop_own/as.numeric(as.character(TotHouseOwner$Value)))
AveHouseRentSize <- data.frame(ZIP = HouseOcc[,2], # place holder, see Below
                              value = pop_rent/as.numeric(as.character(TotHouseRent$Value)))
CensusData$HouseAveSize <- AveHouseSize$value
# CensusData$HouseAveOwnSize <- AveHouseOwnSize$value
# CensusData$HouseAveRentSize <- AveHouseRentSize$value

HouseSize <- read.csv(sprintf("ACS_%s_5YR_B11016_with_ann.csv", Year), header = T)[-1,]
TotHouseSize <- as.numeric(as.character(HouseSize[,which(names(HouseSize) == "HD01_VD01")]))
# - percent family household                  B11016 - HD01_VD02
TotHouseFam <- HouseSize[,c(2, which(names(HouseSize) == "HD01_VD02"))]
names(TotHouseFam) <- c("ZIP", "Value") 
PerHouseFam <- TotHouseFam
PerHouseFam$Value <- as.numeric(as.character(PerHouseFam$Value))/TotHouseSize
CensusData$HousePerFam <- PerHouseFam$Value
# - percent non-family household              B11016 - HD01_VD09
TotHouseNonFam <- HouseSize[,c(2, which(names(HouseSize) == "HD01_VD09"))]
names(TotHouseNonFam) <- c("ZIP", "Value") 
PerHouseNonFam <- TotHouseNonFam
PerHouseNonFam$Value <- as.numeric(as.character(PerHouseNonFam$Value))/TotHouseSize
CensusData$HousePerNonFam <- PerHouseNonFam$Value
# - Household Type by size (Average)          B11016 (more accurate)
persons <- c(2,3,4,5,6,7,1,2,3,4,5,6,7)
cols <- c(8,10,12,14,16,18,22,24,26,28,30,32,34)
pop <- rep(0, nrow(HouseSize))
for (i in 1:length(persons)){
  pop <- pop + persons[i] * as.numeric(as.character(HouseSize[,cols[i]]))
}
AveHouseSize <- data.frame(ZIP = HouseSize[,2],
                           value = pop/TotHouseSize)
CensusData$HouseAveSize <- AveHouseSize$value

# - Household Size by number of vehicles      B08201
HouseVeh <- read.csv(sprintf("ACS_%s_5YR_B08201_with_ann.csv", Year), header = T)[-1,]
TotHouseVeh <- as.numeric(as.character(HouseVeh[,which(names(HouseVeh) == "HD01_VD01")]))
vehicles <- c(0,1,2,3,4)
cols <- c(6,8,10,12,14)
sum_vehicle <- rep(0, nrow(HouseVeh))
for (i in 1:length(vehicles)){
  sum_vehicle <- sum_vehicle + vehicles[i] * as.numeric(as.character(HouseVeh[,cols[i]]))
}
AveHouseVeh <- data.frame(ZIP = HouseVeh[,2],
                          value = sum_vehicle/TotHouseVeh)
CensusData$HouseAveVeh <- AveHouseVeh$value

# - Household Size by number of workers       B08202
HouseWork <- read.csv(sprintf("ACS_%s_5YR_B08202_with_ann.csv", Year), header = T)[-1,]
TotHouseWork <- as.numeric(as.character(HouseWork[,which(names(HouseWork) == "HD01_VD01")]))
workers <- c(0,1,2,3)
cols <- c(6,8,10,12)
sum_worker <- rep(0, nrow(HouseWork))
for (i in 1:length(workers)){
  sum_worker <- sum_worker + workers[i] * as.numeric(as.character(HouseWork[,cols[i]]))
}
AveHouseWork <- data.frame(ZIP = HouseWork[,2],
                           value = sum_worker/TotHouseWork)
CensusData$HouseAveWork <- AveHouseWork$value

# - Median number of rooms                    B25041
HouseRoom <- read.csv(sprintf("ACS_%s_5YR_B25041_with_ann.csv", Year), header = T)[-1,]
TotHouseRoom <- as.numeric(as.character(HouseRoom[,which(names(HouseRoom) == "HD01_VD01")]))
Roomers <- c(0,1,2,3,4,5)
cols <- c(6,8,10,12,14,16)
sum_Roomer <- rep(0, nrow(HouseRoom))
for (i in 1:length(Roomers)){
  sum_Roomer <- sum_Roomer + Roomers[i] * as.numeric(as.character(HouseRoom[,cols[i]]))
}
AveHouseRoom <- data.frame(ZIP = HouseRoom[,2],
                           value = sum_Roomer/TotHouseRoom)
CensusData$HouseAveRoom <- AveHouseRoom$value

# - Median Year household move into the house B25039
HouseYear <- read.csv(sprintf("ACS_%s_5YR_B25039_with_ann.csv", Year), header = T)[-1,]
CensusData$HouseYr <- as.numeric(as.character(HouseYear[,which(names(HouseYear) == "HD01_VD02")]))
CensusData$HouseYrOwn <- as.numeric(as.character(HouseYear[,which(names(HouseYear) == "HD01_VD03")]))
CensusData$HouseYrRent <- as.numeric(as.character(HouseYear[,which(names(HouseYear) == "HD01_VD04")]))

# - Median Year of Structure Built            B25035
HouseConsYear <- read.csv(sprintf("ACS_%s_5YR_B25035_with_ann.csv", Year), header = T)[-1,]
CensusData$HouseConsYr <- as.numeric(as.character(HouseConsYear[,which(names(HouseConsYear) == "HD01_VD01")]))

# - Single family houses (Units by structure) B11011
HouseSfr <- read.csv(sprintf("ACS_%s_5YR_B11011_with_ann.csv", Year), header = T)[-1,]
TotHouseSfr <- as.numeric(as.character(HouseSfr[,which(names(HouseSfr) == "HD01_VD01")]))
TotHouseSfr_1unit <- rep(0, nrow(HouseSfr))
for (col in c(10,20,28,36)){
  TotHouseSfr_1unit <- TotHouseSfr_1unit + as.numeric(as.character(HouseSfr[,col]))
}
CensusData$HousePerSfr <- TotHouseSfr_1unit/TotHouseSfr



# - Median Household Income                      B19013 - HD01_VD01  
IncMed <- read.csv(sprintf("ACS_%s_5YR_B19013_with_ann.csv", Year), header = T)[-1,]
CensusData$IncMed <- rep(NA, nrow(CensusData))
IncMed <- IncMed[,c(2, which(names(IncMed) == "HD01_VD01"))]
for (zip in as.numeric(as.character(IncMed$GEO.id2))){
  if (zip %in% as.numeric(as.character(CensusData$ZIP))){
    CensusData$IncMed[which(as.numeric(as.character(CensusData$ZIP)) == zip)] <- 
      as.numeric(as.character(IncMed$HD01_VD01))[which(as.numeric(as.character(IncMed$GEO.id2)) == zip)]
  }
}
# -- Median Family Income                        B19113 - HD01_VD01 
# -- Per-capita Income                           B19301 - HD01_VD01 (diff row counts)

# - Poverty Status                            B17001 - HD01_VD02
IncPov <- read.csv(sprintf("ACS_%s_5YR_B17001_with_ann.csv", Year), header = T)[-1,]
TotIncPov <- as.numeric(as.character(IncPov[,which(names(IncPov) == "HD01_VD01")]))
TotIncPov_Pov <- as.numeric(as.character(IncPov[,which(names(IncPov) == "HD01_VD02")]))
CensusData$IncPov <- TotIncPov/TotIncPov_Pov

# - GINI Index of Income Inequality           B19083 - HD01_VD01
IncGini <- read.csv(sprintf("ACS_%s_5YR_B19083_with_ann.csv", Year), header = T)[-1,]
CensusData$IncGini <- as.numeric(as.character(IncGini[,which(names(IncGini) == "HD01_VD01")]))

# - Mortgage Status                           B25081 - HD01_VD02
IncMortg <- read.csv(sprintf("ACS_%s_5YR_B25081_with_ann.csv", Year), header = T)[-1,]
TotIncMortg <- as.numeric(as.character(IncMortg[,which(names(IncMortg) == "HD01_VD01")]))
TotIncMortg_with <- as.numeric(as.character(IncMortg[,which(names(IncMortg) == "HD01_VD02")]))
CensusData$IncMortg <- TotIncMortg_with/TotIncMortg

# - Median Gross Rent                         B25064 - HD01_VD01
IncRent <- read.csv(sprintf("ACS_%s_5YR_B25064_with_ann.csv", Year), header = T)[-1,]
CensusData$IncRent <- rep(NA, nrow(CensusData))
IncRent <- IncRent[,c(2, which(names(IncRent) == "HD01_VD01"))]
for (zip in as.numeric(as.character(IncRent$GEO.id2))){
  if (zip %in% as.numeric(as.character(CensusData$ZIP))){
    CensusData$IncRent[which(as.numeric(as.character(CensusData$ZIP)) == zip)] <- 
      as.numeric(as.character(IncRent$HD01_VD01))[which(as.numeric(as.character(IncRent$GEO.id2)) == zip)]
  }
}

# - Median Value                              B25077 - HD01_VD01
IncVal <- read.csv(sprintf("ACS_%s_5YR_B25077_with_ann.csv", Year), header = T)[-1,]
CensusData$IncVal <- rep(NA, nrow(CensusData))
IncVal <- IncVal[,c(2, which(names(IncVal) == "HD01_VD01"))]
for (zip in as.numeric(as.character(IncVal$GEO.id2))){
  if (zip %in% as.numeric(as.character(CensusData$ZIP))){
    CensusData$IncVal[which(as.numeric(as.character(CensusData$ZIP)) == zip)] <- 
      as.numeric(as.character(IncVal$HD01_VD01))[which(as.numeric(as.character(IncVal$GEO.id2)) == zip)]
  }
}


MobReloc <- read.csv(sprintf("ACS_%s_5YR_B07001_with_ann.csv", Year), header = T)[-1,]
TotMobReloc <- as.numeric(as.character(MobReloc[,which(names(MobReloc) == "HD01_VD01")]))
# - Relocation - percent no-move              B07001 - HD01_VD18
TotMobReloc_noMov <- as.numeric(as.character(MobReloc[,which(names(MobReloc) == "HD01_VD18")]))
CensusData$MobRelocNo <- TotMobReloc_noMov/TotMobReloc
# - Relocation - percent same county          B07001 - HD01_VD34
TotMobReloc_Cnty <- as.numeric(as.character(MobReloc[,which(names(MobReloc) == "HD01_VD34")]))
CensusData$MobRelocCty <- TotMobReloc_Cnty/TotMobReloc
# - Relocation - percent same state           B07001 - HD01_VD50
TotMobReloc_Stt <- as.numeric(as.character(MobReloc[,which(names(MobReloc) == "HD01_VD50")]))
CensusData$MobRelocSt <- TotMobReloc_Stt/TotMobReloc
# - Relocation - percent diff state          B07001 - HD01_VD66
TotMobReloc_Nat <- as.numeric(as.character(MobReloc[,which(names(MobReloc) == "HD01_VD66")]))
TotMobReloc_Abd <- as.numeric(as.character(MobReloc[,which(names(MobReloc) == "HD01_VD82")]))
CensusData$MobRelocOut <- TotMobReloc_Nat/TotMobReloc + TotMobReloc_Abd/TotMobReloc

MobMode <- read.csv(sprintf("ACS_%s_5YR_B08301_with_ann.csv", Year), header = T)[-1,]
TotMobMode <- as.numeric(as.character(MobMode[,which(names(MobMode) == "HD01_VD01")]))
# - by car                                    B08301 - HD01_VD02
TotMobModeCar <- as.numeric(as.character(MobMode[,which(names(MobMode) == "HD01_VD02")]))
CensusData$MobPerModeCar <- TotMobModeCar/TotMobMode
# - by public transportation                  B08301 - HD01_VD10
TotMobModePub <- as.numeric(as.character(MobMode[,which(names(MobMode) == "HD01_VD10")]))
CensusData$MobPerModePub <- TotMobModePub/TotMobMode
# - by bike/walk                              B08301 - HD01_VD18, HD02_VD19
TotMobModeBik <- as.numeric(as.character(MobMode[,which(names(MobMode) == "HD01_VD18")]))
TotMobModeWlk <- as.numeric(as.character(MobMode[,which(names(MobMode) == "HD02_VD19")]))
CensusData$MobPerModeBkWk <- TotMobModeBik/TotMobMode + TotMobModeWlk/TotMobMode

# - median travel time to work                B08013 - HD01_VD01
TotMobModeHm <- as.numeric(as.character(MobMode[,which(names(MobMode) == "HD01_VD21")]))
TotWorkNoHm <- data.frame(ZIP = as.numeric(as.character(CensusData$ZIP)),
                          worker = TotMobMode - TotMobModeHm)
MobTt <- read.csv(sprintf("ACS_%s_5YR_B08013_with_ann.csv", Year), header = T)[-1,]
TotMobTt <- as.numeric(as.character(MobTt[,which(names(MobTt) == "HD01_VD01")]))
TotMobTt <- data.frame(ZIP = as.numeric(as.character(MobTt$GEO.id2)),
                       Agg_tt = TotMobTt)
CensusData$MobAveTt <- rep(NA, nrow(CensusData))
for (row in 1:nrow(CensusData)){
  if (TotWorkNoHm$ZIP[row] %in% TotMobTt$ZIP){
    row_tt <- which(TotMobTt$ZIP == TotWorkNoHm$ZIP[row])
    CensusData$MobAveTt[row] <- TotMobTt$Agg_tt[row_tt]/TotWorkNoHm$worker[row]
  }
}

# - travel destination type (categorical)     B08008
MobDes <- read.csv(sprintf("ACS_%s_5YR_B08008_with_ann.csv", Year), header = T)[-1,]
TotMobDes <- as.numeric(as.character(MobDes[,which(names(MobDes) == "HD01_VD03")]))
TotMobDes_Res <- as.numeric(as.character(MobDes[,which(names(MobDes) == "HD01_VD04")]))
TotMobDes_Out <- as.numeric(as.character(MobDes[,which(names(MobDes) == "HD01_VD05")]))
CensusData$MobPerDesRes <- TotMobDes_Res/TotMobDes
CensusData$MobPerDesOut <- TotMobDes_Out/TotMobDes




# - School Enrollmemt                         B14001
EduEnrl <- read.csv(sprintf("ACS_%s_5YR_B14001_with_ann.csv", Year), header = T)[-1,]
TotEduEnrl <- rep(0, nrow(EduEnrl))
for (col in c(8,10,12,14,16)){
  TotEduEnrl <- TotEduEnrl + as.numeric(as.character(EduEnrl[,col]))
}
CensusData$EduPerEnrl <- TotEduEnrl/as.numeric(as.character(TotChild$Value))

EduType <- read.csv(sprintf("ACS_%s_5YR_B14003_with_ann.csv", Year), header = T)[-1,]
# - School Types - private school             B14003 - HD01_VD12, HD01_VD40
EduTypePrv <- as.numeric(as.character(EduType[,which(names(EduType) == "HD01_VD12")])) +
  as.numeric(as.character(EduType[,which(names(EduType) == "HD01_VD40")]))
# - School Types - public school              B14003 - HD01_VD03, HD01_VD31
EduTypePub <- as.numeric(as.character(EduType[,which(names(EduType) == "HD01_VD03")])) +
  as.numeric(as.character(EduType[,which(names(EduType) == "HD01_VD31")]))
CensusData$EduPerTypePrv <- EduTypePrv/(EduTypePrv+EduTypePub)
CensusData$EduPerTypePub <- EduTypePub/(EduTypePrv+EduTypePub)

# - Education Attainment                      S1501
EduAtt <- read.csv(sprintf("ACS_%s_5YR_S1501_with_ann.csv", Year), header = T)[-1,]

### - 2015-2017
EduAttTot <- as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC02")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC08")]))
EduAttBac <- as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC06")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC14")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC15")]))
EduAttHi <- as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC04")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC05")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC06")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC11")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC12")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC13")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC14")])) +
as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC15")]))

### - 2014
EduAttTot <- as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC01")])) +
  as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC07")]))
EduAttTot18 <- as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC01")]))
EduAttTot24 <- as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC07")]))
EduAttBac <- as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC05")])) * 
  EduAttTot18 +
  (as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC13")])) +
  as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC14")]))) * 
  EduAttTot24
EduAttHi <- (as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC03")])) +
  as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC04")])) +
  as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC05")]))) *
  EduAttTot18 +
  (as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC10")])) +
  as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC11")])) +
  as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC12")])) +
  as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC13")])) +
  as.numeric(as.character(EduAtt[,which(names(EduAtt) == "HC01_EST_VC14")]))) * 
  EduAttTot24
CensusData$EduPerAttBac <- EduAttBac/EduAttTot/100
CensusData$EduPerAttHi <- EduAttHi/EduAttTot/100




# - Employment Status                         
Emp <- read.csv(sprintf("ACS_%s_5YR_S2301_with_ann.csv", Year), header = T)[-1,]
# - Labor Force Participation Rate            S2301 - HC02_EST_VC01
CensusData$EmpLfpr <- 
  as.numeric(as.character(Emp[,which(names(Emp) == "HC02_EST_VC01")]))/100
# - Unemployment rate                         S2301 - HC04_EST_VC01
CensusData$EmpUnemp <-
  as.numeric(as.character(Emp[,which(names(Emp) == "HC04_EST_VC01")]))/100
  
EmpOcc <- read.csv(sprintf("ACS_%s_5YR_S2401_with_ann.csv", Year), header = T)[-1,]
EmpOccTot <- as.numeric(as.character(EmpOcc[,which(names(EmpOcc) == "HC01_EST_VC01")]))
# - Occupation - Management, science, art     S2401 - HC01_EST_VC02
EmpOccMgmt <- as.numeric(as.character(EmpOcc[,which(names(EmpOcc) == "HC01_EST_VC02")]))
CensusData$EmpOccMgmt <- EmpOccMgmt/EmpOccTot
# - Occupation - Service                      S2401 - HC01_EST_VC18
EmpOccSrv <- as.numeric(as.character(EmpOcc[,which(names(EmpOcc) == "HC01_EST_VC18")]))
CensusData$EmpOccSrv <- EmpOccSrv/EmpOccTot
# - Occupation - Sales, office                S2401 - HC01_EST_VC26
EmpOccSale <- as.numeric(as.character(EmpOcc[,which(names(EmpOcc) == "HC01_EST_VC26")]))
CensusData$EmpOccSale <- EmpOccSale/EmpOccTot
# - Occupation - Farming, construction        S2401 - HC01_EST_VC29
EmpOccNatCons <- as.numeric(as.character(EmpOcc[,which(names(EmpOcc) == "HC01_EST_VC29")]))
CensusData$EmpOccNatCons <- EmpOccNatCons/EmpOccTot
# - Occupation - Transportation, etc.         S2401 - HC01_EST_VC33
EmpOccTrans <- as.numeric(as.character(EmpOcc[,which(names(EmpOcc) == "HC01_EST_VC33")]))
CensusData$EmpOccTrans <- EmpOccTrans/EmpOccTot



Bus <- read.csv(sprintf("BP_20%s_00CZ1_with_ann.csv", Year), header = T)[-1,]
# - Total Business Establishment              BP_2016_00CZ1 - ESTAB (Business Pattern)
BusEst <- as.numeric(as.character(Bus$ESTAB))
# - Size of estbalishment by employee         BP_2016_00CZ1 - EMP, ESTAB (Business Pattern)
AveBusSize <- as.numeric(as.character(Bus$EMP))/BusEst
# - Average Employment payroll (annual)       BP_2016_00CZ1 - EMP, PAYANN (Business Pattern)
AveBusParl <- as.numeric(as.character(Bus$PAYANN))/as.numeric(as.character(Bus$EMP))
CensusData$BusEst <- rep(NA, nrow(CensusData))
CensusData$BusAveSize <- rep(NA, nrow(CensusData))
CensusData$BusAveParl <- rep(NA, nrow(CensusData))
for (row in 1:nrow(Bus)){
  zip = as.numeric(as.character(Bus$GEO.id2))[row]
  if (zip %in% as.numeric(as.character(CensusData$ZIP))){
    row_census <- which(as.numeric(as.character(CensusData$ZIP)) == zip)
    CensusData$BusEst[row_census] <- BusEst[row]
    CensusData$BusAveSize[row_census] <- AveBusSize[row]
    CensusData$BusAveParl[row_census] <- AveBusParl[row]
  }
}

setwd(sub("Scratch", "Output", RootWD))
write.table(CensusData, sprintf("0103_CensusData20%s_0405.csv", Year), 
            sep = ",",
            row.names = FALSE)

# ---------------- 1.3.2. IMPORT - Census Data, one-period, East Coast  ---------------- 

year = 2016
data = data.frame(zip_code_tabulation_area = zip_ls)

get_census_simple <- function(year, var) {
  data <- getCensus(name = "acs/acs5",
                    vintage = year,
                    vars = paste0("group(", var, ")"),
                    region = "zip code tabulation area:*")
  return(data)
}

data <- data.frame(zip_code_tabulation_area = zip_ls) %>%
  #
  # - Total population
  left_join(get_census_simple(year, "B01003") %>%
              dplyr::select(zip_code_tabulation_area, B01003_001E) %>%
              dplyr::rename(PopTot = B01003_001E), 
            by="zip_code_tabulation_area") %>%
  #
  ## Filter out NA's - this is usually the most comprehensive column
  filter(!is.na(PopTot)) %>%
  #
  # - Population under 18    
  left_join(get_census_simple(year, "B09001") %>%
              dplyr::select(zip_code_tabulation_area, B09001_001E) %>%
              dplyr::rename(TotChild = B09001_001E), 
            by="zip_code_tabulation_area") %>%
  mutate(PopPerChild = TotChild/PopTot) %>%
  #
  # - Population over 65
  left_join(get_census_simple(year, "B06001") %>%
              dplyr::select(zip_code_tabulation_area, B06001_001E, B06001_011E, B06001_012E) %>%
              dplyr::mutate(PopPerOld = (B06001_011E + B06001_012E) / B06001_001E) %>%
              dplyr::select(zip_code_tabulation_area, PopPerOld), 
            by="zip_code_tabulation_area") %>%
  #
  # - Median Age
  left_join(get_census_simple(year, "B01002") %>%
              dplyr::select(zip_code_tabulation_area, B01002_001E) %>%
              dplyr::rename(PopMedAge = B01002_001E) %>%
              dplyr::filter(PopMedAge > 0), 
            by="zip_code_tabulation_area") %>%
  #
  # - Sex Percentage
  left_join(get_census_simple(year, "B01001") %>%
              dplyr::select(zip_code_tabulation_area, B01001_001E, B01001_002E, B01001_026E) %>%
              dplyr::mutate(PopPerMale = B01001_002E/B01001_001E,
                            PopPerFemale = B01001_026E/B01001_001E) %>%
              dplyr::select(zip_code_tabulation_area, PopPerMale, PopPerFemale), 
            by="zip_code_tabulation_area") %>%
  #
  # - Race Percentage
  left_join(get_census_simple(year, "B02001") %>%
              dplyr::select(zip_code_tabulation_area, B02001_001E:B02001_006E) %>%
              dplyr::mutate(PopPerWhite = B02001_002E/B02001_001E,
                            PopPerBlack = B02001_003E/B02001_001E,
                            PopPerAmInd = B02001_004E/B02001_001E,
                            PopPerAsian = B02001_005E/B02001_001E,
                            PopPerHawai = B02001_006E/B02001_001E) %>%
              dplyr::select(zip_code_tabulation_area, PopPerWhite, PopPerBlack, 
                            PopPerAmInd, PopPerAsian, PopPerHawai), 
            by="zip_code_tabulation_area") %>%
  #
  # - Marital Status
  left_join(get_census_simple(year, "B06008") %>%
              dplyr::select(zip_code_tabulation_area, B06008_001E:B06008_006E) %>%
              dplyr::mutate(PopPerNever = B06008_002E/B06008_001E,
                            PopPerMarSep = B06008_003E/B06008_001E,
                            PopPerDiv = B06008_004E/B06008_001E,
                            PopPerSep = B06008_005E/B06008_001E,
                            PopPerWid = B06008_006E/B06008_001E) %>%
              dplyr::select(zip_code_tabulation_area, PopPerNever, PopPerMarSep, 
                            PopPerDiv, PopPerSep, PopPerWid), 
            by="zip_code_tabulation_area") %>%
  #
  # - Occupancy Types
  left_join(get_census_simple(year, "B07013") %>%
              dplyr::select(zip_code_tabulation_area, B07013_001E:B07013_003E) %>%
              dplyr::mutate(HousePerOwner = B07013_002E/B07013_001E,
                            HousePerRent = B07013_003E/B07013_001E) %>%
              dplyr::select(zip_code_tabulation_area, HousePerOwner, HousePerRent), 
            by="zip_code_tabulation_area") %>%
  #
  # - Household family type
  left_join(get_census_simple(year, "B11016") %>%
              dplyr::select(zip_code_tabulation_area, B11016_001E, B11016_002E, B11016_009E) %>%
              dplyr::mutate(HousePerFam = B11016_002E/B11016_001E,
                            HousePerNonFam = B11016_009E/B11016_001E) %>%
              dplyr::select(zip_code_tabulation_area, HousePerFam, HousePerNonFam), 
            by="zip_code_tabulation_area") %>%
  #
  # - Household Average Size
  # - Household Average Size by vehicle
  left_join(get_census_simple(year, "B08201") %>%
              dplyr::select(zip_code_tabulation_area, B08201_001E:B08201_006E,
                            B08201_007E, B08201_013E, B08201_019E, B08201_025E) %>%
              dplyr::mutate(HouseAveVeh = (B08201_003E*1 + B08201_004E*2 + 
                                             B08201_005E*3 + B08201_006E*4)/B08201_001E,
                            HouseAveSize = (B08201_007E*1 + B08201_013E*2 + 
                                              B08201_019E*3 + B08201_025E*4)/B08201_001E) %>%
              dplyr::select(zip_code_tabulation_area, HouseAveSize, HouseAveVeh), 
            by="zip_code_tabulation_area") %>%
  #
  # - Household Average Size by worker
  left_join(get_census_simple(year, "B08202") %>%
              dplyr::select(zip_code_tabulation_area, B08202_001E:B08202_005E) %>%
              dplyr::mutate(HouseAveWork = (B08202_003E*1 + B08202_004E*2 + 
                                              B08202_005E*3)/B08202_001E) %>%
              dplyr::select(zip_code_tabulation_area, HouseAveWork), 
            by="zip_code_tabulation_area") %>%
  #
  # - Median number of rooms
  left_join(get_census_simple(year, "B25041") %>%
              dplyr::select(zip_code_tabulation_area, B25041_001E:B25041_007E) %>%
              dplyr::mutate(HouseAveRoom = (B25041_003E*1 + B25041_004E*2 + B25041_005E*3 + 
                                              B25041_006E*4 + B25041_007E*5)/B25041_001E) %>%
              dplyr::select(zip_code_tabulation_area, HouseAveRoom), 
            by="zip_code_tabulation_area") %>%
  #
  # - Median year household move into the unit
  left_join(get_census_simple(year, "B25039") %>%
              dplyr::select(zip_code_tabulation_area, B25039_001E, B25039_002E, B25039_003E) %>%
              dplyr::rename(HouseYr = B25039_001E, 
                            HouseYrOwn = B25039_002E, 
                            HouseYrRent = B25039_003E) %>%
              dplyr::filter(HouseYr > 0) %>%
              dplyr::filter(HouseYrOwn > 0) %>%
              dplyr::filter(HouseYrRent > 0), 
            by="zip_code_tabulation_area") %>%
  #
  # - Median year structure built
  left_join(get_census_simple(year, "B25035") %>%
              dplyr::select(zip_code_tabulation_area, B25035_001E) %>%
              dplyr::rename(HouseConsYr = B25035_001E) %>%
              dplyr::filter(HouseConsYr > 0), 
            by="zip_code_tabulation_area")  %>%
  #
  # - Median Household Income
  left_join(get_census_simple(year, "B19013") %>%
              dplyr::select(zip_code_tabulation_area, B19013_001E) %>%
              dplyr::rename(IncMed = B19013_001E) %>%
              dplyr::filter(IncMed > 0), 
            by="zip_code_tabulation_area") %>%
  #
  # - Poverty Status
  left_join(get_census_simple(year, "B17001") %>%
              dplyr::select(zip_code_tabulation_area, B17001_001E, B17001_002E) %>%
              dplyr::rename(IncPov = B17001_002E/B17001_001E) %>%
              dplyr::select(zip_code_tabulation_area, IncPov), 
            by="zip_code_tabulation_area") %>%
  #
  # - GINI index
  left_join(get_census_simple(year, "B19083") %>%
              dplyr::select(zip_code_tabulation_area, B19083_001E) %>%
              dplyr::rename(IncGini = B19083_001E) %>%
              dplyr::filter(IncGini > 0), 
            by="zip_code_tabulation_area") %>%
  #
  # - Mortgage Status
  left_join(get_census_simple(year, "B25081") %>%
              dplyr::select(zip_code_tabulation_area, B25081_001E, B25081_002E) %>%
              dplyr::rename(IncMortg = B25081_002E/B25081_001E) %>%
              dplyr::select(zip_code_tabulation_area, IncMortg), 
            by="zip_code_tabulation_area") %>%
  #
  # - Median grocc rent
  left_join(get_census_simple(year, "B25064") %>%
              dplyr::select(zip_code_tabulation_area, B25064_001E) %>%
              dplyr::rename(IncRent = B25064_001E) %>%
              dplyr::filter(IncRent > 0), 
            by="zip_code_tabulation_area") %>%
  #
  # - Median value
  left_join(get_census_simple(year, "B25077") %>%
              dplyr::select(zip_code_tabulation_area, B25077_001E) %>%
              dplyr::rename(IncVal = B25077_001E) %>%
              dplyr::filter(IncVal > 0), 
            by="zip_code_tabulation_area") %>%
  #
  # - Relocation
  left_join(get_census_simple(year, "B07001") %>%
              dplyr::select(zip_code_tabulation_area, B07001_001E, B07001_017E, B07001_033E, 
                            B07001_049E, B07001_065E, B07001_081E) %>%
              dplyr::mutate(MobRelocNo = B07001_017E/B07001_001E,
                            MobRelocCty = B07001_033E/B07001_001E,
                            MobRelocSt = B07001_049E/B07001_001E,
                            MobRelocOut = (B07001_065E+B07001_081E)/B07001_001E) %>%
              dplyr::select(zip_code_tabulation_area, MobRelocNo, MobRelocCty, 
                            MobRelocSt, MobRelocOut), 
            by="zip_code_tabulation_area") %>%
  #
  # - Commute mode
  left_join(get_census_simple(year, "B08301") %>%
              dplyr::select(zip_code_tabulation_area, B08301_001E, B08301_002E, B08301_010E, 
                            B08301_018E, B08301_019E) %>%
              dplyr::mutate(MobPerModeCar = B08301_002E/B08301_001E,
                            MobPerModePub = B08301_010E/B08301_001E,
                            MobPerModeBkWk = (B08301_018E + B08301_019E)/B08301_001E) %>%
              dplyr::select(zip_code_tabulation_area, MobPerModeCar, MobPerModePub, 
                            MobPerModeBkWk), 
            by="zip_code_tabulation_area") %>%
  #
  # - Average TT to work
  left_join(get_census_simple(year, "B08013") %>%
              dplyr::select(zip_code_tabulation_area, B08013_001E) %>%
              dplyr::rename(MobAveTt = B08013_001E) %>%
              dplyr::filter(MobAveTt > 0), 
            by="zip_code_tabulation_area") %>%
  #
  # - Work location type
  left_join(get_census_simple(year, "B08008") %>%
              dplyr::select(zip_code_tabulation_area, B08008_002E, B08008_003E, B08008_004E) %>%
              dplyr::mutate(MobPerDesRes = B08008_003E/B08008_002E,
                            MobPerDesOut = B08008_004E/B08008_002E) %>%
              dplyr::select(zip_code_tabulation_area, MobPerDesRes, MobPerDesOut), 
            by="zip_code_tabulation_area")  %>%
  #
  # - school enrollment
  left_join(get_census_simple(year, "B14001") %>%
              dplyr::select(zip_code_tabulation_area, B14001_002E) %>%
              dplyr::rename(enrChild = B14001_002E), 
            by="zip_code_tabulation_area") %>%
  mutate(EduPerEnrl = case_when(
    TotChild == 0 ~ 1,
    enrChild >= TotChild ~ 1,
    enrChild < TotChild ~ enrChild/TotChild
  )) %>%
  #
  # - school type
  left_join(get_census_simple(year, "B14003") %>%
              dplyr::select(zip_code_tabulation_area, B14003_003E, B14003_012E,
                            B14003_031E, B14003_040E) %>%
              dplyr::mutate(
                totEnr = B14003_003E + B14003_012E + B14003_031E + B14003_040E,
                EduPerTypePub = (B14003_003E + B14003_031E) / totEnr,
                EduPerTypePrv = (B14003_012E + B14003_040E) / totEnr
              ) %>%
              dplyr::select(zip_code_tabulation_area, EduPerTypePub, EduPerTypePrv), 
            by="zip_code_tabulation_area") %>%
  #
  # - Education attainment
  left_join(get_census_simple(year, "B06009") %>%
              dplyr::select(zip_code_tabulation_area, B06009_001E:B06009_006E) %>%
              dplyr::mutate(
                EduPerAttBac = (B06009_005E + B06009_006E) / B06009_001E,
                EduPerAttHi = (B06009_001E - B06009_002E) / B06009_001E
              ) %>%
              dplyr::select(zip_code_tabulation_area, EduPerAttBac, EduPerAttHi), 
            by="zip_code_tabulation_area")  %>%
  #
  # - Labor force participation rate
  # - Unemployment rate
  left_join(get_census_simple(year, "B17005") %>%
              dplyr::select(zip_code_tabulation_area, B17005_001E, B17005_004E, B17005_009E, B17005_015E, B17005_020E,
                            B17005_006E, B17005_011E, B17005_017E, B17005_022E) %>%
              dplyr::mutate(
                LaborForce = B17005_004E + B17005_009E + B17005_015E + B17005_020E,
                umEmp = B17005_006E + B17005_011E + B17005_017E + B17005_022E,
                EmpLfpr = LaborForce / B17005_001E,
                EmpUnemp = umEmp / LaborForce
              ) %>%
              dplyr::select(zip_code_tabulation_area, EmpLfpr, EmpUnemp), 
            by="zip_code_tabulation_area") %>%
  #
  # - Occupation
  left_join(get_census_simple(year, "B08124") %>%
              dplyr::select(zip_code_tabulation_area, B08124_001E:B08124_006E) %>%
              dplyr::mutate(
                EmpOccMgmt = B08124_002E / B08124_001E,
                EmpOccSrv = B08124_003E / B08124_001E,
                EmpOccSale = B08124_004E / B08124_001E,
                EmpOccNatCons = B08124_005E / B08124_001E,
                EmpOccTrans = B08124_006E / B08124_001E
              ) %>%
              dplyr::select(zip_code_tabulation_area, EmpOccMgmt, EmpOccSrv, 
                            EmpOccSale, EmpOccNatCons, EmpOccTrans), 
            by="zip_code_tabulation_area") %>%
  #
  ## - Post-processing
  dplyr::select(-c("TotChild", "enrChild"))

# Business

write.table(data_anls, "0103_Census_2012-2016_230316.csv", row.names = FALSE, sep = ",")


# ---------------- 1.4.1. IMPORT - Location Variables for NC ---------------- 

setwd(paste(RootWD, "/NC_Spatial/Intersect", sep=""))
LocVariables <- data.frame(sort(read.csv("ZipBdry.csv", header = T)[,2]))
names(LocVariables) <- "ZIP"


## AMENITIES

ZIP_Ref <- read.csv("ZipBdry.csv", header = T)[,1:2]

# Beach Access
BeachAccess <- read.csv("BeachAccess.csv", header = T)[,c(33,7:20)]
for (col in c(2,4,5,6,7,8,9,11,12,13,14,15)) {
  Entry <- rep(0, nrow(BeachAccess))
  Entry[which(BeachAccess[,col] %in% c("Yes", "Y"))] <- 1
  BeachAccess[,col] <- Entry
}
for (col in c(3,10)) {
  Entry <- rep(0, nrow(BeachAccess))
  Mean <- mean(BeachAccess[,col])
  Entry[which(BeachAccess[,col] > Mean)] <- 1
  BeachAccess[,col] <- Entry
}
BeachAccess$Qual <- rep(NA, nrow(BeachAccess))
for (row in 1:nrow(BeachAccess)) {
  BeachAccess$Qual[row] <- sum(BeachAccess[row,2:15])
}
BeachAcc <- data.frame(unique(BeachAccess$NEAR_FID))
BeachAcc$Qual <- rep(NA, nrow(BeachAcc))
names(BeachAcc) <- c("NEAR_FID", "Qual")
for (row in 1:nrow(BeachAcc)) {
  BeachAcc$Qual[row] <- sum(BeachAccess[which(BeachAccess$NEAR_FID == 
                                                BeachAcc$NEAR_FID[row]),]$Qual)
}
BeachAcc$Qual <- BeachAcc$Qual + 1
LocVarRows <- which(LocVariables$ZIP %in% 
                      ZIP_Ref[which(ZIP_Ref$FID %in% 
                                      unique(BeachAcc$NEAR_FID)),]$ZIP)
LocVariables$AmnBeach <- rep(0, nrow(LocVariables))
for (row in LocVarRows) {
  FID_entry <- ZIP_Ref[which(ZIP_Ref$ZIP == LocVariables$ZIP[row]),]$FID
  LocVariables$AmnBeach[row] <- BeachAcc[which(BeachAcc$NEAR_FID == FID_entry),
                                         ]$Qual
}
# CAMA_SIGN, PARKING, PRKG_STR, DUNE_WALK, OVERLOOK, HAND_DUNE, HAND_OVRLK,
# HAND_BEACH, HAND_PRKG, RESTRM, SHOWER, PIC_TABLE, VEH_ACCESS, BIKE_RACK

# Beating Access
BoatAccess <- read.csv("BoatingAccess.csv", header = T)[,c(30,11,14:17,20:26)]
for (col in c(2:6,12:13)) {
  Entry <- rep(0, nrow(BoatAccess))
  Entry[which(BoatAccess[,col] %in% c("PAVED", "Y"))] <- 1
  BoatAccess[,col] <- Entry
}
for (col in c(7:11)) {
  Entry <- rep(0, nrow(BoatAccess))
  Mean <- mean(BoatAccess[,col])
  Entry[which(BoatAccess[,col] > Mean)] <- 1
  BoatAccess[,col] <- Entry
}
BoatAccess$Qual <- rep(NA, nrow(BoatAccess))
for (row in 1:nrow(BoatAccess)) {
  BoatAccess$Qual[row] <- sum(BoatAccess[row,2:13])
}
BoatAccess$Qual <- BoatAccess$Qual + 1
BoatAcc <- data.frame(unique(BoatAccess$ZIP))
BoatAcc$Qual <- rep(NA, nrow(BoatAcc))
names(BoatAcc) <- c("ZIP", "Qual")
for (row in 1:nrow(BoatAcc)) {
  BoatAcc$Qual[row] <- sum(BoatAccess[which(BoatAccess$ZIP == 
                                              BoatAcc$ZIP[row]),]$Qual)
}
LocVariables$AmnBoat <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% BoatAcc$ZIP)) {
  LocVariables$AmnBoat[row] <- BoatAcc[which(BoatAcc$ZIP == LocVariables$ZIP[row]),
                                         ]$Qual
}
# SDV_RECOM, CANOE, RESTROOM, TRASH_CAN, ADA_ACCESS, FIX_DOCK, FLOAT_DOCK,
# LNCH_LANE, PKNG_TRLV, PKNG_SGLV, PKNG_COVER, LIGHTS

# Public Fishing Area
FishArea <- read.csv("FishNear.csv", header = T)
FishArea <- FishArea[FishArea$NEAR_DIST<0.0016,]
# within walking distance (1 mile) of a zip code
LocVariables$AmnFish <- rep(0, nrow(LocVariables))
LocVariables[LocVariables$ZIP %in% unique(FishArea$ZIP),]$AmnFish <- 1

# Coastal Plain Paddle Trail
Paddle <- read.csv("PaddleTrail.csv", header = T)[,c(7,13,16,19,22,23)]
Paddle[Paddle>1000] <- 0
LocVariables$AmnPadl <- rep(0, nrow(LocVariables))
ZIP_Padl <- ZIP_Ref[which(ZIP_Ref$FID %in% unique(Paddle$NEAR_FID)),]$ZIP
for (row in which(LocVariables$ZIP %in% ZIP_Padl)) {
  FID <- ZIP_Ref[which(ZIP_Ref$ZIP == LocVariables$ZIP[row]),]$FID
  LocVariables$AmnPadl[row] <- sum(Paddle[which(Paddle$NEAR_FID == FID), 1:5], 
                                   nrow(Paddle[which(Paddle$NEAR_FID == FID), 1:5]))
}

# Game land
GameLd <- read.csv("GameLd.csv", header = T)
GameLd <- GameLd[-c(nrow(GameLd), nrow(GameLd)-2, nrow(GameLd)-4),15:16]
GameLd$POLY_AREA <- 0.000247105 * GameLd$POLY_AREA
LocVariables$AmnGmld <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in%  unique(GameLd$ZIP))) {
  LocVariables$AmnGmld[row] <- sum(GameLd[which(GameLd$ZIP == 
                                                  LocVariables$ZIP[row]),]$POLY_AREA)
}




## NATURAL

# High Quality Water
HiQualWtr <- read.csv("HiQualWtr.csv", header = T)[,c(16,17)]
HiQualWtr$POLY_AREA <- 0.000247105 * HiQualWtr$POLY_AREA
LocVariables$EnvHiqwtr <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in%  unique(HiQualWtr$ZIP))) {
  LocVariables$EnvHiqwtr[row] <- sum(HiQualWtr[which(HiQualWtr$ZIP == 
                                                  LocVariables$ZIP[row]),]$POLY_AREA)
}

# Water Quality
# WtrQual <- read.csv("WtrQual.csv", header = T)

# Erosion 2004
Ers04 <- read.csv("Erosion04.csv", header = T)[,c(16,7,12)]
ZIP_Ers04 <- ZIP_Ref[which(ZIP_Ref$FID %in% unique(Ers04$NEAR_FID)),]$ZIP
LocVariables$EnvErs04 <- rep(NA, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% ZIP_Ers04)) {
  FID <- ZIP_Ref[which(ZIP_Ref$ZIP == LocVariables$ZIP[row]),]$FID
  LocVariables$EnvErs04[row] <- sum(Ers04[which(Ers04$NEAR_FID == FID),]$SMTH_RATE * 
                                      Ers04[which(Ers04$NEAR_FID == FID),]$Shape_Leng) / 
    sum(Ers04[which(Ers04$NEAR_FID == FID),]$Shape_Leng)
}

# Erosion 2013
Ers13 <- read.csv("Erosion13.csv", header = T)[,c(24,19,23)]
ZIP_Ers13 <- ZIP_Ref[which(ZIP_Ref$FID %in% unique(Ers13$NEAR_FID)),]$ZIP
LocVariables$EnvErs <- rep(NA, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% ZIP_Ers13)) {
  FID <- ZIP_Ref[which(ZIP_Ref$ZIP == LocVariables$ZIP[row]),]$FID
  LocVariables$EnvErs[row] <- sum(Ers13[which(Ers13$NEAR_FID == FID),]$SMTH_RATE * 
                                      Ers13[which(Ers13$NEAR_FID == FID),]$Shape_Le_1) / 
    sum(Ers13[which(Ers13$NEAR_FID == FID),]$Shape_Le_1)
}

# Erosion change
LocVariables$EnvErsChange <- LocVariables$EnvErs13 - LocVariables$EnvErs04
LocVariables <- LocVariables[,-8]

# Coastal Plants
VegNear <- read.csv("ShoreVegNear.csv", header = T)[,c(1,26,24,28)]
VegInts <- read.csv("ShoreVegInts.csv", header = T)[,c(2,30,25,31)]
names(VegNear) <- c("OID", "ZIP", "Type", "Length")
names(VegInts) <- c("OID", "ZIP", "Type", "Length")
for (row in 1:nrow(VegNear)) {
  VegNear$ZIP[row] <- ZIP_Ref[which(ZIP_Ref$FID == VegNear$ZIP[row]),]$ZIP
}
VegInts$OID <- 1 + VegInts$OID
ShoreVeg <- rbind(VegInts, VegNear[which(!(VegNear$OID %in% VegInts$OID)),])
VegMarsh <- ShoreVeg[which(ShoreVeg$Type == "Marsh"),]
LocVariables$EnvVegMarsh <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% VegMarsh$ZIP)) {
  LocVariables$EnvVegMarsh[row] <- sum(VegMarsh[which(VegMarsh$ZIP == LocVariables$ZIP[row]),]$Length)
}
VegSedbank <- ShoreVeg[which(ShoreVeg$Type == "Sediment Bank"),]
LocVariables$EnvVegSedbank <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% VegSedbank$ZIP)) {
  LocVariables$EnvVegSedbank[row] <- sum(VegSedbank[which(VegSedbank$ZIP == LocVariables$ZIP[row]),]$Length)
}
VegMod <- ShoreVeg[which(ShoreVeg$Type == "Modified"),]
LocVariables$EnvVegMod <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% VegMod$ZIP)) {
  LocVariables$EnvVegMod[row] <- sum(VegMod[which(VegMod$ZIP == LocVariables$ZIP[row]),]$Length)
}
VegSwamp <- ShoreVeg[which(ShoreVeg$Type == "Swamp Forest"),]
LocVariables$EnvVegSwamp <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% VegSwamp$ZIP)) {
  LocVariables$EnvVegSwamp[row] <- sum(VegSwamp[which(VegSwamp$ZIP == LocVariables$ZIP[row]),]$Length)
}
VegMis <- ShoreVeg[which(ShoreVeg$Type == "Miscellaneous"),]
LocVariables$EnvVegMis <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% VegMis$ZIP)) {
  LocVariables$EnvVegMis[row] <- sum(VegMis[which(VegMis$ZIP == LocVariables$ZIP[row]),]$Length)
}


## SHORELINE FACILITIES

# Shoreline Structure
ShoreStrucX <- read.csv("ShoreStrucX.csv", header = T)[,c(29,26)]
FacBrg <- ShoreStrucX[which(ShoreStrucX$STRUC_NAM == "Bridge"),]
LocVariables$FacShoreBrg <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% FacBrg$ZIP)) {
  LocVariables$FacShoreBrg[row] <- nrow(FacBrg[which(FacBrg$ZIP == LocVariables$ZIP[row]),])
}
FacPier <- ShoreStrucX[which(ShoreStrucX$STRUC_NAM == "Pier - Floating Dock - Wharf"),]
LocVariables$FacShorePier <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% FacPier$ZIP)) {
  LocVariables$FacShorePier[row] <- nrow(FacPier[which(FacPier$ZIP == LocVariables$ZIP[row]),])
}

ShoreStrucY <- read.csv("ShoreStrucY.csv", header = T)[,c(31,28,25)]
FacRamp <- ShoreStrucY[which(ShoreStrucY$STRUC_NAM == "Boat Ramp"),]
LocVariables$FacShoreRamp <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% FacRamp$ZIP)) {
  LocVariables$FacShoreRamp[row] <- sum(FacRamp[which(FacRamp$ZIP == LocVariables$ZIP[row]),]$LENGTH)
}
FacBrkwt <- ShoreStrucY[which(ShoreStrucY$STRUC_NAM == "Breakwater"),]
LocVariables$FacShoreBrkwt <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% FacBrkwt$ZIP)) {
  LocVariables$FacShoreBrkwt[row] <- sum(FacBrkwt[which(FacBrkwt$ZIP == LocVariables$ZIP[row]),]$LENGTH)
}
FacJty <- ShoreStrucY[which(ShoreStrucY$STRUC_NAM == "Groin - Jetty"),]
LocVariables$FacShoreJty <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% FacJty$ZIP)) {
  LocVariables$FacShoreJty[row] <- sum(FacJty[which(FacJty$ZIP == LocVariables$ZIP[row]),]$LENGTH)
}
FacSil <- ShoreStrucY[which(ShoreStrucY$STRUC_NAM == "Sill"),]
LocVariables$FacShoreSil <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% FacSil$ZIP)) {
  LocVariables$FacShoreSil[row] <- sum(FacSil[which(FacSil$ZIP == LocVariables$ZIP[row]),]$LENGTH)
}
FacRrp <- ShoreStrucY[which(ShoreStrucY$STRUC_NAM == "Sloped - Riprap"),]
LocVariables$FacShoreRrp <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% FacRrp$ZIP)) {
  LocVariables$FacShoreRrp[row] <- sum(FacRrp[which(FacRrp$ZIP == LocVariables$ZIP[row]),]$LENGTH)
}
FacBkh <- ShoreStrucY[which(ShoreStrucY$STRUC_NAM == "Vertical Structure - Bulkhead"),]
LocVariables$FacShoreBkh <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% FacBkh$ZIP)) {
  LocVariables$FacShoreBkh[row] <- sum(FacBkh[which(FacBkh$ZIP == LocVariables$ZIP[row]),]$LENGTH)
}




## FACILITIES

# Emergency center
EmerCentrNear <- read.csv("EmerCentrNear.csv", header = T)[,c(2,4)]
EmerCentrNear <- EmerCentrNear %>% mutate(rank_order = min_rank(ZIP)) %>% arrange(rank_order)
LocVariables$FacDisEmerCtr <- EmerCentrNear$NEAR_DIST

# Emergency Shelter
EmerShelterNear <- read.csv("EmerShelterNear.csv", header = T)[,c(2,4)]
EmerShelterNear <- EmerShelterNear %>% mutate(rank_order = min_rank(ZIP)) %>% arrange(rank_order)
LocVariables$FacDisEmerShlt <- EmerShelterNear$NEAR_DIST

# University
UnivDis <- read.csv("UnivNear.csv", header = T)[,c(2,4)]
UnivLoc <- read.csv("Univ.csv", header = T)$ZIP
UnivDis <- UnivDis %>% mutate(rank_order = min_rank(ZIP)) %>% arrange(rank_order)
for (row in which(UnivDis$ZIP %in% UnivLoc)) {
  UnivDis$NEAR_DIST[row] <- -length(which(UnivLoc == UnivDis$ZIP[row]))
}
LocVariables$FacDisUniv <- UnivDis$NEAR_DIST

# Public School
PuSclDis <- read.csv("PubSchlNear.csv", header = T)[,c(2,4)]
PuSclLoc <- read.csv("PubSchlItrs.csv", header = T)$ZIP
PuSclDis <- PuSclDis %>% mutate(rank_order = min_rank(ZIP)) %>% arrange(rank_order)
for (row in which(PuSclDis$ZIP %in% PuSclLoc)) {
  PuSclDis$NEAR_DIST[row] <- -length(which(PuSclLoc == PuSclDis$ZIP[row]))
}
LocVariables$FacDisPuScl <- PuSclDis$NEAR_DIST

# Nonpublic School
NonpSclDis <- read.csv("NonpubSchlNear.csv", header = T)[,c(2,4)]
NonpSclLoc <- read.csv("NonpubSchlItrs.csv", header = T)$ZIP
NonpSclDis <- NonpSclDis %>% mutate(rank_order = min_rank(ZIP)) %>% arrange(rank_order)
for (row in which(NonpSclDis$ZIP %in% NonpSclLoc)) {
  NonpSclDis$NEAR_DIST[row] <- -length(which(NonpSclLoc == NonpSclDis$ZIP[row]))
}
LocVariables$FacDisNonpScl <- NonpSclDis$NEAR_DIST

# Public Library
LibNear <- read.csv("PubLibNear.csv", header = T)[,c(2,4)]
LibNear <- LibNear %>% mutate(rank_order = min_rank(ZIP)) %>% arrange(rank_order)
LocVariables$FacDisLib <- LibNear$NEAR_DIST

# Correctional Institutes
CorrNear <- read.csv("CorrNear.csv", header = T)[,c(2,4)]
CorrNear <- CorrNear %>% mutate(rank_order = min_rank(ZIP)) %>% arrange(rank_order)
LocVariables$FacDisCorr <- CorrNear$NEAR_DIST




## REGION

# Housing Zone 2018
HouseZone <- read.csv("HouseZone.csv", header = T)[,c(25,4,26)]
LocVariables$RegHouse <- rep(NA, nrow(LocVariables))
for (row in 1:nrow(LocVariables)) {
  LocVariables$RegHouse[row] <- HouseZone[which(HouseZone$ZIP == 
                                                  LocVariables$ZIP[row]),
                                          ][which.max(HouseZone[which(HouseZone$ZIP == 
                                                                        LocVariables$ZIP[row]),
                                                                ]$AREA_GEO),
                                            ]$District
}
LocVariables$RegHouse <- as.factor(LocVariables$RegHouse)

# Urban Region
UrbDist <- read.csv("UrbDist.csv", header = T)[,2:4]
UrbRef <- read.csv("UrbName.csv", header = T)[,c(1,10,4)]
UrbDist <- UrbDist %>% mutate(rank_order = min_rank(ZIP)) %>% arrange(rank_order)
UrbDist$RegUrbName <- rep(NA, nrow(UrbDist))
UrbDist$RegUrbSize <- rep(NA, nrow(UrbDist))
for (row in 1:nrow(UrbDist)) {
  row_ref <- which(UrbRef$FID == UrbDist$NEAR_FID[row])
  UrbDist$RegUrbName[row] <- as.character(UrbRef$NAME[row_ref])
  UrbDist$RegUrbSize[row] <- UrbRef$POP_EST[row_ref]
}
LocVariables$RegUrbName <- UrbDist$RegUrbName
LocVariables$RegUrbSize <- UrbDist$RegUrbSize
LocVariables$RegUrbName <- as.factor(LocVariables$RegUrbName)



# LAND COVER

# distance to closest urban area
LocVariables$LcDistUrb <- UrbDist$NEAR_DIST

# Land Cover
LdCov <- rbind(t(read.csv("LdCov_1.csv", header = T))[-c(1,2),],
               t(read.csv("LdCov_2.csv", header = T))[-c(1,2),],
               t(read.csv("LdCov_3.csv", header = T))[-c(1,2),],
               t(read.csv("LdCov_4.csv", header = T))[-c(1,2),],
               t(read.csv("LdCov_5.csv", header = T))[-c(1,2),],
               t(read.csv("LdCov_6.csv", header = T))[-c(1,2),])
LdCov <- as.data.frame(LdCov)
names(LdCov) <- c("LcPerWtr", "LcPerDevOp", "LcPerDevLow", "LcPerDevMed", "LcPerDevHi", "LcPerBar", 
                  "LcPerForDec", "LcPerForEv", "LcPerForMx", "LcPerSrb", "LcPerGrs", "LcPerPast", 
                  "LcPerCorp", "LcPerWoodWetl", "LcPerHerbWetl")
LdCov$Tot <- apply(LdCov, 1, sum)
LdCov$ZIP <- as.numeric(substr(row.names(LdCov), start = 3, stop = 7))
for (col in 1:15) {
  LdCov[,col] <- LdCov[,col]/LdCov$Tot
}
for (row in 1:length(LocVariables$ZIP[which(!(LocVariables$ZIP %in% LdCov$ZIP))])) {
  LdCov <- rbind(LdCov, rep(NA, ncol(LdCov)))
  LdCov$ZIP[nrow(LdCov)] <- LocVariables$ZIP[which(!(LocVariables$ZIP %in% LdCov$ZIP))][row]
}
LdCov <- unique(LdCov)
LdCov <- LdCov %>% mutate(rank_order = min_rank(ZIP)) %>% arrange(rank_order)
LocVariables <- cbind(LocVariables, LdCov[1:15])

## PARCEL

# LANDVAL, PARVAL, MZIP, PARVALTYPE, OWNTYPE, Shape_Length (ft), Shape_Area (sqft)
Parcels_50 <- read.csv("Parcels_50.csv", header = T)
Parcels_56 <- read.csv("Parcels_56.csv", header = T)
Parcels_59 <- read.csv("Parcels_59.csv", header = T)
'
Parcels_50$MZIP <- as.character(Parcels_50$MZIP)
Parcels_50$LANDVAL <- as.numeric(Parcels_50$LANDVAL)
Parcels_50 <- Parcels_50[which((nchar(Parcels_50$MZIP) < 11) & 
(nchar(Parcels_50$MZIP) > 4) &
(substr(Parcels_50$MZIP, start = 1, stop = 2) %in% c("27", "28"))),]
Parcels_50$MZIP <- as.numeric(substr(Parcels_50$MZIP, start = 1, stop = 5))
Parcels_50 <- Parcels_50[-which(is.na(Parcels_50$MZIP)),]
'
Parcels_50 <- Parcels_50[which(Parcels_50$NEAR_DIST == 0),]
Parcels_50 <- Parcels_50[,c(71,6,7,26,27,69,70)]
Parcels_50$LANDVAL <- as.numeric(Parcels_50$LANDVAL)

Parcels_56 <- Parcels_56[which(Parcels_56$NEAR_DIST == 0),]
Parcels_56 <- Parcels_56[,c(71,6,7,26,27,69,70)]
Parcels_56$LANDVAL <- as.numeric(Parcels_56$LANDVAL)

Parcels_59 <- Parcels_59[which(Parcels_59$NEAR_DIST == 0),]
Parcels_59 <- Parcels_59[,c(71,6,7,26,27,69,70)]
Parcels_59$LANDVAL <- as.numeric(Parcels_59$LANDVAL)

Parcels <- rbind(Parcels_50, Parcels_56, Parcels_59)
Parcels$LSI <- (0.3048*Parcels$Shape_Length)/(2*sqrt(pi*(0.092903*Parcels$Shape_Area)))

LocVariables$ParCnt <- rep(NA, nrow(LocVariables))
LocVariables$ParMnArea <- rep(NA, nrow(LocVariables))
LocVariables$ParMnLdVal <- rep(NA, nrow(LocVariables))
LocVariables$ParMnParVal <- rep(NA, nrow(LocVariables))
LocVariables$ParValType <- rep(NA, nrow(LocVariables))
LocVariables$ParOwnType <- rep(NA, nrow(LocVariables))
LocVariables$ParGeoLSI <- rep(NA, nrow(LocVariables))
LocVariables$ParGeoLPI <- rep(NA, nrow(LocVariables))

for (row in 1:nrow(LocVariables)) {
  pars <- Parcels[which(Parcels$NEAR_FID == 
                             ZIP_Ref[which(ZIP_Ref$ZIP == LocVariables$ZIP[row]),]$FID),]
  LocVariables$ParCnt[row] <- nrow(pars)
  LocVariables$ParMnArea[row] <- mean(pars$Shape_Area)
  LocVariables$ParMnLdVal[row] <- sum(pars$LANDVAL*pars$Shape_Area)/sum(pars$Shape_Area)
  LocVariables$ParMnParVal[row] <- sum(pars$PARVAL*pars$Shape_Area)/sum(pars$Shape_Area)
  LocVariables$ParValType[row] <- names(which.max(summary(pars$PARVALTYPE)))
  LocVariables$ParOwnType[row] <- names(which.max(summary(pars$OWNTYPE)))
  LocVariables$ParGeoLSI[row] <- mean(pars$LSI)
  LocVariables$ParGeoLPI[row] <- max(pars$Shape_Area)/sum(pars$Shape_Area)
}
LocVariables$ParGeoLPI[which(LocVariables$ParGeoLPI < 0)] <- NA
LocVariables$ParCnt[which(LocVariables$ParCnt == 0)] <- NA
LocVariables$ParValType <- as.factor(LocVariables$ParValType)
LocVariables$ParOwnType[which(LocVariables$ParOwnType == "TAXABLE")] <- "Taxable"
LocVariables$ParOwnType[which(LocVariables$ParOwnType == "PRIVATE")] <- "Private"
LocVariables$ParOwnType <- as.factor(LocVariables$ParOwnType)




## RISK PERCEPTION

# Inlet Hazard Area 1978
IHA78 <- read.csv("InlHazArea.csv", header = T)[,c(15,13)]
LocVariables$RiskIHA78 <- rep(0, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% IHA78$ZIP)) {
  LocVariables$RiskIHA78[row] <- sum(IHA78[which(IHA78$ZIP == LocVariables$ZIP[row]),]$AREA_ACRES)
}


# Set Back Factor
SBF <- read.csv("SetBackFac_no88.csv", header = T)[,c(110,112,88,66,13,34,53,107)]
names(SBF) <- c("FID", "Len", "1979", "1983", "1997", "2004", "2011", "2013")
SBF_PC <- princomp(SBF[,3:8])
as.numeric((summary(SBF_PC)$sdev)^2/6)
scores <- as.data.frame(SBF_PC$scores)[,c(1,2)]
cor2m(SBF[,3:8],scores)
scores[,1] <- scores[,1]*(cor2m(SBF[,3:8],scores)[3,1]/abs(cor2m(SBF[,3:8],scores)[3,1]))
scores[,2] <- scores[,2]*(cor2m(SBF[,3:8],scores)[5,2]/abs(cor2m(SBF[,3:8],scores)[5,2]))
SBF_PC <- cbind(SBF[,c(1,2)], scores)
names(SBF_PC) <- c("FID", "Len", "MnSBF", "DelSBF")
for (row in 1:nrow(SBF_PC)) {
  SBF_PC$ZIP[row] <- ZIP_Ref[which(ZIP_Ref$FID == SBF_PC$FID[row]),]$ZIP
}
LocVariables$RiskMnSBF <- rep(NA, nrow(LocVariables))
LocVariables$RiskDelSBF <- rep(NA, nrow(LocVariables))
for (row in which(LocVariables$ZIP %in% SBF_PC$ZIP)) {
  sbf_row <- SBF_PC[which(SBF_PC$ZIP == LocVariables$ZIP[row]),]
  LocVariables$RiskMnSBF[row] <- sum(sbf_row$MnSBF*sbf_row$Len)/sum(sbf_row$Len)
  LocVariables$RiskDelSBF[row] <- sum(sbf_row$DelSBF*sbf_row$Len)/sum(sbf_row$Len)
}

# Save
write.table(LocVariables, "0104_LocationVariables_0419.csv",
            sep = ",", row.names = FALSE)


# ---------------- 1.4.2. IMPORT - Location Variables for U.S. ---------------- 

##### 

skip <- try(entry<- opq(bbox = bb) %>% # package: osmdata
              add_osm_feature(key='natural', value='beach') %>%
              osmdata_sf()) 
if (inherits(possibleError, "error")) next

save_loc = paste0(RootWD, "/OSM_features/beach")

bb = matrix(NA, 2, 2)
t = 0
df_points = NULL
df_lines = NULL
df_poly = NULL
df_multp = NULL
for (x in -98:-67){
  for (y in 25:47){
    bb[1,1] = x
    bb[1,2] = x+1
    bb[2,1] = y
    bb[2,2] = y+1
    print(t)
    print(bb)
    #
    entry = ""
    tryCatch({entry = opq(bbox = bb) %>% # package: osmdata
      add_osm_feature(key='natural', value='beach') %>%
      osmdata_sf()}, error = function(e){})
    if (length(entry) == 1) {next}
    #
    df_points = rbind(df_points, entry$osm_points %>% dplyr::select(geometry))
    df_lines = rbind(df_lines, entry$osm_lines %>% dplyr::select(geometry))
    df_poly = rbind(df_poly, entry$osm_polygons %>% dplyr::select(geometry))
    df_multp = rbind(df_multp, entry$osm_multipolygons %>% dplyr::select(geometry))
    #
    if (t %% 10 == 0 & t != 0) {
      st_write(df_points, paste0(save_loc, "/points_", str_pad(t %/% 10, 3, pad="0"), ".shp"))
      st_write(df_lines, paste0(save_loc, "/lines_", str_pad(t %/% 10, 3, pad="0"), ".shp"))
      st_write(df_poly, paste0(save_loc, "/polys_", str_pad(t %/% 10, 3, pad="0"), ".shp"))
      st_write(df_multp, paste0(save_loc, "/multipolys_", str_pad(t %/% 10, 3, pad="0"), ".shp"))
      #
      df_points = NULL
      df_lines = NULL
      df_poly = NULL
      df_multp = NULL
      #
      t = t + 1
    }
  }
}

try
