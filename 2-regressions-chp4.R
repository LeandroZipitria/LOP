unlink(".RData")

# ----------------------------------------------------------------------------
# ------------------------------- Regressions --------------------------------
# ----------------------------------------------------------------------------

# dbf original database
# dfP price difference database

library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy
library("lfe", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy
#library(data.table)



regression <- function(x) {
  
  sink("regressions-2020-diff-v44EJ.txt", append = T)
  print(str(x))
  print(dim(x))
  sink()
  

  #### Variety regression --
  
  # Base regression, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DVar + SameChain + DifCity
                      | Product + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-diff-v44EJ.txt", append = T)
  print("---------- Third column -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing store fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DVar
                      | Product + SuperL + SuperR + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-diff-v44EJ.txt", append = T)
  print("---------- Fourth column -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing store pair fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DVar
                       | Time + ChainR * Product + ChainL * Product | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-diff-v44EJ.txt", append = T)
  print("---------- Second column -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing store pair fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DVar
                       | Time + ChainR * Product + ChainL * Product + SuperL + SuperR | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-diff-v44EJ.txt", append = T)
  print("---------- Second column -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing store pair fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DVar
                       | Time * ChainR + Product + ChainL * Time + SuperL + SuperR | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020-diff-v44EJ.txt", append = T)
  print("---------- Second column -----------")
  print(reg1, include.rownames=F)
  print(reg1$N, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  

}


##----- Panel A) Random database -------------------

# dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/Old/2018_PriceDiff-random-2021.rds") #ClusterUy
# gc()
# 
# sink("regressions-2020-diff-v44EJ.txt", append = T)
# print("---------- RANDOM database -----------")
# sink()
# dfP$DVar <- as.numeric(as.factor(dfP$DVar))
# regression(dfP)


##----- Panel B) Year 2011 -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/Old/df2011-inst.rds") #ClusterUy
gc()

sink("regressions-2020-diff-v44EJ.txt", append = T)
print("---------- YEAR 2011 database -----------")
sink()
dfP$DVar <- as.numeric(as.factor(dfP$DVar))
regression(dfP)



##----- 30 kilometers -------------------
# 
# dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/PriceDiff_complete.rds") #ClusterUy
# dfP <- setDT(dfP)[dfP$Distance <= log(31),] # 30 kilometers
# dfP$DVar <- as.numeric(as.character(dfP$DVar))
# gc()
# 
# sink("regressions-2020-diff-v44EJ.txt", append = T)
# print("---------- 30 kilometers -----------")
# sink()
# gc()
# 
# regression(dfP)
# 
# 
# ##----- 15 kilometers -------------------
# 
# dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/PriceDiff_complete.rds") #ClusterUy
# dfP <- setDT(dfP)[dfP$Distance <= log(16),] # 15 kilometers
# dfP$DVar <- as.numeric(as.character(dfP$DVar))
# gc()
# 
# sink("regressions-2020-diff-v44EJ.txt", append = T)
# print("---------- 15 kilometers -----------")
# sink()
# gc()
# 
# regression(dfP)
# 
# 
# ##----- 10 kilometers -------------------
# 
# dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/PriceDiff_complete.rds") #ClusterUy
# dfP <- setDT(dfP)[dfP$Distance <= log(11),] # 10 kilometers
# dfP$DVar <- as.numeric(as.character(dfP$DVar))
# gc()
# 
# sink("regressions-2020-diff-v44EJ.txt", append = T)
# print("---------- 10 kilometers -----------")
# sink()
# gc()
# 
# regression(dfP)
# 
# 
# ##----- 5 kilometers -------------------
# 
# dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/PriceDiff_complete.rds") #ClusterUy
# dfP <- setDT(dfP)[dfP$Distance <= log(6),] # 5 kilometers
# dfP$DVar <- as.numeric(as.character(dfP$DVar))
# gc()
# 
# sink("regressions-2020-diff-v44EJ.txt", append = T)
# print("---------- 5 kilometers -----------")
# sink()
# gc()
# 
# regression(dfP)
# 
# 
# ##----- Montevideo only -------------------
# 
# dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/PriceDiff_complete.rds") #ClusterUy
# dfP <- setDT(dfP)[dfP$CityL == 30 & dfP$CityR == 30,] # Both stores in Montevideo
# dfP$DVar <- as.numeric(as.character(dfP$DVar))
# gc()
# 
# sink("regressions-2020-diff-v44EJ.txt", append = T)
# print("---------- Montevideo only -----------")
# sink()
# gc()
# 
# regression(dfP)

## Erase difference database
rm(dfP)


###### ----------------- End of script --------------------------------
