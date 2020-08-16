unlink(".RData")

# ----------------------------------------------------------------------------
# ------------------------------- Regressions --------------------------------
# ----------------------------------------------------------------------------

# dbf original database
# dfP price difference database

library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy
library("lfe", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy
#library(data.table)


###### -------------- Table 5 (Price Differences) --------------

# All regressions are the same, so we apply a function to two dataframes

regression <- function(x) {
  
  sink("regressions-2020-v43.txt", append = T)
  print(str(x))
  print(dim(x))
  sink()
  
  #### Distance regression
  
  # Base regression, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance
                       | Product + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-v43.txt", append = T)
  print("---------- Distancia sin controles -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  
  # Base regression, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain
                       | Product + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-v43.txt", append = T)
  print("---------- Distancia con controles cadena -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  

  
  # Absorbing store fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance
                       | Product + SuperL + SuperR + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-v43.txt", append = T)
  print("---------- Distancia con controles super dummies -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  #### Variety regression
  
  # Absorbing store fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ DVar
                       | Product + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-v43.txt", append = T)
  print("---------- solo variedad -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Base regression, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ DVar + DifCity + SameChain
                       | Product + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-v43.txt", append = T)
  print("---------- con control par de super sin distancia -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing store fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ DVar
                       | Product + SuperL + SuperR + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-v43.txt", append = T)
  print("---------- con controles dummy super y sin distancia -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Base regression, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DVar + DifCity + SameChain
                       | Product + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-v43.txt", append = T)
  print("---------- distancia + variedad con par supers -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing store fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DVar
                       | Product + SuperL + SuperR + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-v43.txt", append = T)
  print("---------- distancia + variedad con super dummies -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
}


##----- Panel A) Random database -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/Old/2018_PriceDiff-random.rds") #ClusterUy
gc()

sink("regressions-2020-v43.txt", append = T)
print("---------- RANDOM database -----------")
sink()
dfP$DVar <- as.numeric(as.character(dfP$DVar))
regression(dfP)


##----- Panel B) Year 2011 -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/Old/df2011-inst.rds") #ClusterUy
gc()

sink("regressions-2020-v43.txt", append = T)
print("---------- YEAR 2011 database -----------")
sink()
dfP$DVar <- as.numeric(as.character(dfP$DVar))
regression(dfP)


###### -------------- Table 6 (Price differences by distance) --------------

##----- Panel A) 30 kilometers -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/Old/2018_PriceDiff.rds") #ClusterUy
dfP <- setDT(dfP)[dfP$Distance <= log(31),] # 30 kilometers
dfP$DVar <- as.numeric(as.character(dfP$DVar))
gc()

sink("regressions-2020-v43.txt", append = T)
print("---------- 30 kilometers -----------")
sink()
gc()

regression(dfP)

##----- Panel B) More than 100 kilometers -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/Old/2018_PriceDiff.rds") #ClusterUy
dfP <- setDT(dfP)[dfP$Distance > log(101),] # more than 100 kilometers
dfP$DVar <- as.numeric(as.character(dfP$DVar))
gc()

sink("regressions-2020-v43.txt", append = T)
print("---------- +100 kilometers -----------")
sink()
gc()

regression(dfP)

## Erase difference database
rm(dfP)


##----- Montevideo only -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/Old/2018_PriceDiff.rds") #ClusterUy
dfP <- setDT(dfP)[dfP$CityL == 30 & dfP$CityR == 30,] # Both stores in Montevideo
dfP$DVar <- as.numeric(as.character(dfP$DVar))
gc()

sink("regressions-2020-v43.txt", append = T)
print("---------- Montevideo only -----------")
sink()
gc()

regression(dfP)

## Erase difference database
rm(dfP)


###### ----------------- End of script --------------------------------
