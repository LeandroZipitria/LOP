unlink(".RData")
# load packages
library("lfe", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## Cluster
library("dplyr", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## Cluster
library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## Cluster
#library(lfe)

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/2020-PriceDiff.rds") #ClusterUy
#dfP <- readRDS("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/less200metters.rds")
#dfP <- readRDS("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/Less10km.rds")

str(dfP)

# Merge instrument
dfPinst <-  fread("/clusteruy/home/leandroz/Bases/Border/2020-dbase-levels-final.csv", data.table = F)
dfPinst <- dfPinst[,c("Super","Product","Time","Instrument")]
dfP$SuperL <- as.numeric(as.character(dfP$SuperL))
dfP$SuperR <- as.numeric(as.character(dfP$SuperR))
dfPinst$Time <- as.factor(dfPinst$Time) 
dfPinst$Product <- as.factor(dfPinst$Product)

dfP <- setDT(dfP)[setDT(dfPinst), on=c("Time", "Product", "SuperR" = "Super"), InstR:= i.Instrument] 
dfP <- setDT(dfP)[setDT(dfPinst), on=c("Time", "Product", "SuperL" = "Super"), InstL:= i.Instrument] 
gc()

sink("salidaV40-2019-12-erase-merge.txt", append = T)
print("---------- Base Regression ER (clustered Store + time) -----------")
print(sum(is.na(dfP$InstL)), include.rownames=F)
print(sum(is.na(dfP$InstR)), include.rownames=F)
sink()
sum(is.na(dfP$InstL))
sum(is.na(dfP$InstR))

head(dfP)
dfP$DInst <- abs(dfP$InstR - dfP$InstL)
dfP <- dfP[,c("Time","Product","DifPrice","DVar","Distance","CityL","CityR",
              "ChainL","ChainR","SuperL","SuperR","DifCity","SameChain","DInst")]

head(dfP)
str(dfP)
saveRDS(dfP, "/clusteruy/home/leandroz/Bases/Border/2020-PriceDiff-final.rds")


# Create random database 10%
set.seed(777)
a = round(0.1*nrow(dfP), digits = 0)
dfP2 <- dfP[sample(nrow(dfP), a),]
str(dfP2)
saveRDS(dfP2, "/clusteruy/home/leandroz/Bases/Border/2020-PriceDiff-random.rds")
rm(dfP2)


# Year 2011
y2011 <- c(46:57)
dfP3 <- dfP[dfP$Time %in% y2011,]
saveRDS(dfP3, "/clusteruy/home/leandroz/Bases/Border/df2011-inst-2020.rds")
rm(dfP3)

