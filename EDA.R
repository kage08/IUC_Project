
# ---------------- 0.1. SET UP - Packages and WD ---------------- 

# install.packages("ZillowR")
library(ZillowR) # Cite
# install.packages("assertthat")
# library(assertthat)
# install.packages("devtools")
# library(devtools)
# devtools::install_github("xiyuansun/realEstAnalytics")
library(realEstAnalytics) # Cite
set_zillow_web_service_id('X1-ZWz17e71z5vswb_3mmvk')
library(ggplot2)
# install.packages("rnoaa")
library(rnoaa) # Cite
# usethis::edit_r_environ()
Sys.setenv(NOAA_KEY = "rGBcYgZXrRSAvjncsfFEQQLXyYnjdIYI")
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
# install.packages("zoo")
library(zoo)
# install.packages("dplyr")
library(dplyr)
# install.packages("GGally")
library(GGally)
library(RColorBrewer)
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
# install.packages("censusapi")
library(censusapi)

## Bayesian Analysis
# install.packages("rstan", source = TRUE)
library(rstan)
# install.packages("rstanarm", source = TRUE)
library(rstanarm)
# install.packages("rstantools", source = TRUE)
library(rstantools)

Sys.setenv(CENSUS_KEY="bef9711de6b9166f981c672e3bd7d0c9569e9a89")
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")

RootWD <- getwd()

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

# ---------------- 2.2.3.* EXPLORATORY - Price Difference over storm time (All ZHVI) ---------------- 

# Import from 1.
StormData <- read.csv("0101_StormData_230306.csv", sep = ",")
StormTime <- read.csv("0101_StormTime_230306.csv", sep = ",")
AllZRI <- read.csv("0102_ZRI_230306.csv", header = T)
AllZHVI <- read.csv("0102_ZHVI_230306.csv", header = T) %>%
  dplyr::mutate(ZIP = RegionName) # for 2.2.3

# Format storm time file to be joined to the storm data
StormTime = StormTime[,-1] %>%
  dplyr::mutate(
    Start = floor(Start/10000),
    End = floor(End/10000)
  )
StormTime_proc = StormTime %>% 
  dplyr::filter(Start == End) %>%
  dplyr::rename(Month = Start) %>%
  dplyr::select(Event, Month)
StormTime_toProc = StormTime %>% 
  filter(Start != End)
for (row in 1:nrow(StormTime_toProc)) {
  entry = data.frame(
    Event = StormTime_toProc$Event[row],
    Month = StormTime_toProc$Start[row]:StormTime_toProc$End[row]
  )
  StormTime_proc = rbind(StormTime_proc, entry)
}
# Process stormdata file to be joined with the time
StormComp = StormData[,-1] %>%
  reshape2::melt(id.var = 'ZIP') %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(variable = as.numeric(substr(variable, 2, 7))) %>%
  dplyr::rename(Event = variable) %>%
  # Join the two files
  full_join(StormTime_proc, by="Event") %>%
  dplyr::rename(Initial = Month, 
                Storm = value)

# Make ZHVI time series price change table
AllZHVI = AllZHVI[,c(10:249, 1)] # Exclude dates after Dec. 2019
for (int_mo in c(1:60)){ # intervals in months
  # Housing Price Change Dataframe
  AllZHVI_Price <- data.frame(matrix(nrow = nrow(AllZHVI),
                                     ncol = ncol(AllZHVI) - int_mo - 1)) 
  for (row in 1:nrow(AllZHVI_Price)){
    if (row %% 1000 == 0) {
      print(paste(int_mo, ": ", row, sep=""))
    }
    for (col in 1:ncol(AllZHVI_Price)){
      AllZHVI_Price[row,col] <- 
        as.numeric(AllZHVI[row,col+int_mo+1]) - 
        as.numeric(AllZHVI[row,col+1])
    }
  }
  AllZHVI_Price$ZIP <- AllZHVI$ZIP
  names(AllZHVI_Price)[1:(ncol(AllZHVI_Price)-1)] <-
    names(AllZHVI)[1:(ncol(AllZHVI_Price)-1)]
  Entry <- reshape2::melt(AllZHVI_Price, id.var='ZIP')
  Entry$Interval <- rep(int_mo, nrow(Entry))
  names(Entry) <- c("ZIP", "Initial", "Change", "Interval")
  write.table(Entry, 
              paste0("020203_StormPriceTemporal/ZHVI_", int_mo, ".csv"), 
              row.names=FALSE, sep=",")
}

# Combine all ZHVI values
for (int_mo in c(1:60)){
  print(int_mo)
  Entry = read.csv(paste0("020203_StormPriceTemporal/ZHVI_", int_mo, ".csv"))
  dataAnls = Entry %>%
    mutate(
      Initial = paste0(substr(Initial, 2, 5), substr(Initial, 7, 8)),
      Initial = as.numeric(Initial)
    ) %>%
    left_join(StormComp, by=c("ZIP", "Initial")) %>%
    mutate(Storm = ifelse(is.na(Storm), 0, Storm)) %>%
    filter(Initial >= 200808) # Storm data only starts at Aug. 2008
  write.table(dataAnls, 
              paste0("020203_StormPriceTemporal/ZHVI_", int_mo, ".csv"), 
              row.names=FALSE, sep=",")
}


# ---------------- 2.4.2. EXPLORATORY - Census Correlation var heatmap ---------------- 

# Import Data from 1.3
setwd(gsub("Scratch", "Output", RootWD))
Census2014 <- read.csv("0103_CensusData2014_0406.csv", header = T)[,-1]
Census2015 <- read.csv("0103_CensusData2015_0406.csv", header = T)[,-1]
Census2016 <- read.csv("0103_CensusData2016_0406.csv", header = T)[,-1]
Census2017 <- read.csv("0103_CensusData2017_0406.csv", header = T)[,-1]
CensusData <- rbind(Census2014, Census2015, Census2016, Census2017)
CensusData <- CensusData[-which(CensusData$EduPerEnrl > 100),]
# Correlation matrix
CorVar <- as.data.frame(matrix(nrow = ncol(CensusData),
                                   ncol = ncol(CensusData)))
colnames(CorVar) <- rownames(CorVar) <- names(CensusData)
CorVarP <- CorVar
for (col in 1:ncol(CensusData)) {
  for (row in 1:ncol(CensusData)) {
    CorVar[row,col] <- cor.test(CensusData[,row], CensusData[,col])$estimate
    CorVarP[row,col] <- cor.test(CensusData[,row], CensusData[,col])$p.value
  }
}
# Plot
coul <- colorRampPalette(rev(brewer.pal(8, "RdYlBu")))(100)
heatmap(as.matrix(CorVar), Colv = NA, Rowv = NA, scale = "none", col = coul)
legend(x="right", legend=c("-1.0", "-0.9", "-0.8", "-0.7", "-0.6", "-0.5", "-0.4", "-0.3", "-0.2", "-0.1",
                           "0.0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0"), 
       fill=colorRampPalette(rev(brewer.pal(8, "RdYlBu")))(21))

# Save Correlation matrix
setwd(sub("Scratch", "Output", RootWD))
write.table(CorVar, "0204_CorMtxVarUnclean_0406.csv", 
            sep = ",",
            row.names = FALSE)
write.table(CorVarP, "0204_CorMtxVarPUnclean_0406.csv", 
            sep = ",",
            row.names = FALSE)


# ---------------- 3.2. ANOVA - Time lag for storm ----------------

##### ZHVI

States = c("NC", "NY", "SC", "GA", "FL", 
           "ME", "VT", "NH", "MA", "RI", "CT", "NJ", "DE", "MD", 
           "PA", "VA", "AL", "MS", "LA", "TX",
           "ALL")
for (State in States) {
  # ANOVA for ZHVI
  ZHVI_anova <- as.data.frame(matrix(nrow = 6, ncol = 0))
  rownames(ZHVI_anova) <- c("34-0", "50-0", "64-0", "50-34", "64-34", "64-50")
  ZHVI_anova_P <- ZHVI_anova
  for (int_mo in c(1:60)) {
    Data <- read.csv(paste0("020203_StormPriceTemporal/ZHVI_", int_mo, ".csv")) %>%
      left_join(AllZHVI[,c(1,5)], by="ZIP")
    # Clear out things that go out of the study period
    COVID_date = 201912
    due_date = COVID_date - 100*int_mo%/%12 - int_mo %% 12
    Data = Data[Data$Initial <= due_date,]
    # Continue with the rest...
    if (State != "ALL") {
      Data <- Data[Data$StateName == State,]
    }
    if (length(unique(Data$Storm)) < 2) {
      next
    }
    anova.mod <- aov(Change ~ as.factor(Storm), data = Data)
    TukeyHSD(anova.mod)
    ZHVI_anova$entry <- TukeyHSD(anova.mod)$`as.factor(Storm)`[,1]
    ZHVI_anova_P$entry <- TukeyHSD(anova.mod)$`as.factor(Storm)`[,4]
    names(ZHVI_anova)[ncol(ZHVI_anova)] <- int_mo
    names(ZHVI_anova_P)[ncol(ZHVI_anova_P)] <- int_mo
  }
  
  write.table(ZHVI_anova, 
              paste0("0302_ZHVI_anova/0302_ZHVI_anova_test_", State, "_230325.csv"),
              sep = ",",
              row.names = TRUE)
  write.table(ZHVI_anova_P, 
              paste0("0302_ZHVI_anova/0302_ZHVI_anova_P_test_", State, "_230325.csv"),
              sep = ",",
              row.names = TRUE)
  
  # Plot - ZHVI anova
  if (length(ZHVI_anova_P) == 0) {
    next
  }
  ZHVI_anova_plot <- ZHVI_anova
  for (col in 1:ncol(ZHVI_anova_plot)) {
    for (row in 1:nrow(ZHVI_anova_plot)) {
      if (ZHVI_anova_P[row,col]>0.025){
        ZHVI_anova_plot[row,col] <- NA
      }
    }
  }
  bar = max(abs(ZHVI_anova_plot[!is.na(ZHVI_anova_plot)]))
  ZHVI_anova_plot$del <- c(bar, bar/2, 0, 0, -bar/2, -bar)
  coul <- colorRampPalette(rev(brewer.pal(8, "Spectral")))(100)
  png(file=paste0("0302_ZHVI_anova/0302_ZHVI_anova_plot_", State, ".png"), 
      width=500, height=500)
  heatmap(as.matrix(ZHVI_anova_plot), 
          
          
          
          Colv = NA, Rowv = NA, scale = "none", col = coul)
  legend(x="right", 
         legend=round(c(bar, bar/2, 0, -bar/2, -bar)),
         fill=colorRampPalette(brewer.pal(8, "Spectral"))(5))
  dev.off()
}


##### ZRI

##### Scratch

# ANOVA for ZRI
ZRI_anova <- as.data.frame(matrix(nrow = 6, ncol = 0))
rownames(ZRI_anova) <- c("34-0", "50-0", "64-0", "50-34", "64-34", "64-50")
ZRI_anova_P <- ZRI_anova
for (Int in unique(AllZRI$Interval)) {
  Data <- AllZRI[which(AllZRI$Interval == Int),]
  anova.mod <- aov(Change ~ as.factor(Storm), data = Data)
  TukeyHSD(anova.mod)
  ZRI_anova$entry <- TukeyHSD(anova.mod)$`as.factor(Storm)`[,1]
  ZRI_anova_P$entry <- TukeyHSD(anova.mod)$`as.factor(Storm)`[,4]
  names(ZRI_anova)[ncol(ZRI_anova)] <- Int
  names(ZRI_anova_P)[ncol(ZRI_anova_P)] <- Int
}


# t-test data
ZRI_t_Test <- as.data.frame(matrix(nrow = 0, 
                                   ncol = length(unique(AllZRI$Interval))))
names(ZRI_t_Test) <- c("1", "2", "3", "4", "5", "6", "7", 
                       "8", "9", "10","11","12","18","24")
ZHVI_t_Test <- as.data.frame(matrix(nrow = 0, 
                                   ncol = length(unique(AllZHVI$Interval))))
names(ZHVI_t_Test) <- c("1", "2", "3", "4", "5", "6", "7", 
                       "8", "9", "10","11","12","18","24")
AllZRI[AllZRI$Storm != "0",]$Storm <- "1"
AllZHVI[AllZHVI$Storm != "0",]$Storm <- "1"

# t-test for ZRI
tTest <- c()
for (Int in unique(AllZRI$Interval)) {
  Data <- AllZRI[which(AllZRI$Interval == Int),]
  Entry <- t.test(Data[Data$Storm == "0",]$Change, Data[Data$Storm == "1",]$Change,
                  alternative = "greater")
  tTest <- append(tTest, Entry$p.value)
}
ZRI_tTest <- as.data.frame(t(tTest))
names(ZRI_tTest) <- names(ZRI_anova)
ZRI_t_Test <- rbind(ZRI_t_Test, ZRI_tTest)

# t-test for ZHVI
tTest <- c()
for (Int in unique(AllZHVI$Interval)) {
  Data <- paste0("020203_StormPriceTemporal/ZHVI_", int_mo, ".csv")
  Entry <- t.test(Data[Data$Storm == "0",]$Change, Data[Data$Storm == "1",]$Change,
                  alternative = "greater")
  tTest <- append(tTest, Entry$p.value)
}
ZHVI_tTest <- as.data.frame(t(tTest))
names(ZHVI_tTest) <- names(ZHVI_anova)
ZHVI_t_Test <- rbind(ZHVI_t_Test, ZHVI_tTest)

# t-test [50-64] data
AllZRI <- read.csv("0202_StormPriceZRI_Full_0513.csv", header = T)[,-1] # for 2.2.3
AllZHVI <- read.csv("0202_StormPriceZHVI_Full_0513.csv", header = T)[,-1] # for 2.2.3
AllZRI[!(AllZRI$Storm %in% c("50","64")),]$Storm <- "0"
AllZRI[AllZRI$Storm %in% c("50","64"),]$Storm <- "1"
AllZHVI[!(AllZHVI$Storm %in% c("50","64")),]$Storm <- "0"
AllZHVI[AllZHVI$Storm %in% c("50","64"),]$Storm <- "1"

# t-test [50-64] for ZRI
tTest <- c()
for (Int in unique(AllZRI$Interval)) {
  Data <- AllZRI[which(AllZRI$Interval == Int),]
  Entry <- t.test(Data[Data$Storm == "0",]$Change, Data[Data$Storm == "1",]$Change,
                  alternative = "greater")
  tTest <- append(tTest, Entry$p.value)
}
ZRI_tTest <- as.data.frame(t(tTest))
names(ZRI_tTest) <- names(ZRI_anova)
ZRI_t_Test <- rbind(ZRI_t_Test, ZRI_tTest)

# t-test [50-64] for ZHVI
tTest <- c()
for (Int in unique(AllZHVI$Interval)) {
  Data <- AllZHVI[which(AllZHVI$Interval == Int),]
  Entry <- t.test(Data[Data$Storm == "0",]$Change, Data[Data$Storm == "1",]$Change,
                  alternative = "greater")
  tTest <- append(tTest, Entry$p.value)
}
ZHVI_tTest <- as.data.frame(t(tTest))
names(ZHVI_tTest) <- names(ZHVI_anova)
ZHVI_t_Test <- rbind(ZHVI_t_Test, ZHVI_tTest)


# t-test [severe storm] data
AllZRI <- read.csv("0202_StormPriceZRI_Full_0513.csv", header = T)[,-1] # for 2.2.3
AllZHVI <- read.csv("0202_StormPriceZHVI_Full_0513.csv", header = T)[,-1] # for 2.2.3
AllZRI[AllZRI$Storm != "64",]$Storm <- "0"
AllZHVI[AllZHVI$Storm != "64",]$Storm <- "0"

# t-test [severe storm] for ZRI
tTest <- c()
for (Int in unique(AllZRI$Interval)) {
  Data <- AllZRI[which(AllZRI$Interval == Int),]
  Entry <- t.test(Data[Data$Storm == "0",]$Change, Data[Data$Storm == "64",]$Change,
                  alternative = "greater")
  tTest <- append(tTest, Entry$p.value)
}
ZRI_tTest <- as.data.frame(t(tTest))
names(ZRI_tTest) <- names(ZRI_anova)
ZRI_t_Test <- rbind(ZRI_t_Test, ZRI_tTest)

# t-test [severe storm] for ZHVI
tTest <- c()
for (Int in unique(AllZHVI$Interval)) {
  Data <- AllZHVI[which(AllZHVI$Interval == Int),]
  Entry <- t.test(Data[Data$Storm == "0",]$Change, Data[Data$Storm == "64",]$Change,
                  alternative = "greater")
  tTest <- append(tTest, Entry$p.value)
}
ZHVI_tTest <- as.data.frame(t(tTest))
names(ZHVI_tTest) <- names(ZHVI_anova)
ZHVI_t_Test <- rbind(ZHVI_t_Test, ZHVI_tTest)


# Save all processed data
setwd(gsub("Scratch", "Output", RootWD))
write.table(ZRI_anova, "0302_ZRI_anova_0513.csv",
            sep = ",",
            row.names = FALSE)
write.table(ZRI_anova_P, "0302_ZRI_anova_P_0513.csv",
            sep = ",",
            row.names = FALSE)
write.table(ZRI_t_Test, "0302_ZRI_tTest_0513.csv",
            sep = ",",
            row.names = FALSE)

write.table(ZHVI_t_Test, "0302_ZHVI_tTest_0513.csv",
            sep = ",",
            row.names = FALSE)

# Plot - ZRI anova
ZRI_anova_plot <- ZRI_anova
for (col in 1:ncol(ZRI_anova_plot)) {
  for (row in 1:nrow(ZRI_anova_plot)) {
    if (ZRI_anova_P[row,col]>0.025){
      ZRI_anova_plot[row,col] <- NA
    }
  }
}
ZRI_anova_plot$del <- c(-100,-50,0,0,50,100)
coul <- colorRampPalette(rev(brewer.pal(8, "Spectral")))(100)
heatmap(as.matrix(ZRI_anova_plot), Colv = NA, Rowv = NA, scale = "none", col = coul)
legend(x="bottom", 
       legend = c(60,30,0,-30,-60),
       fill=colorRampPalette(brewer.pal(8, "Spectral"))(5))


# Plot - ZRI t-test
ZRI_t_plot <- ZRI_t_Test
ZRI_t_plot[ZRI_t_plot>0.025] <- 1
ZRI_t_plot[ZRI_t_plot<=0.025] <- 0
coul <- colorRampPalette(brewer.pal(8, "Blues"))(4)[c(3,1)]
heatmap(as.matrix(ZRI_t_plot), Colv = NA, Rowv = NA, scale = "none", col = coul)
legend(x="bottom",legend = c("Significant Reduction", "No Significance"), fill=coul)

# Plot - ZHVI t-test
ZHVI_t_plot <- ZHVI_t_Test
ZHVI_t_plot[ZHVI_t_plot>0.025] <- 1
ZHVI_t_plot[ZHVI_t_plot<=0.025] <- 0
coul <- colorRampPalette(brewer.pal(8, "Blues"))(4)[c(3,1)]
heatmap(as.matrix(ZHVI_t_plot), Colv = NA, Rowv = NA, scale = "none", col = coul)
legend(x="bottom",legend = c("Significant Reduction", "No Significance"), fill=coul)







