unlink(".RData")
# load packages
library("lfe", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## Cluster
library("dplyr", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## Cluster

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff.rds") #ClusterUy
str(dfP)

### Transform variables using as.factor()
dfP$SameChain <- ifelse(dfP$ChainR == dfP$ChainL, 1,0)
dfP$Product <- as.factor(dfP$Product)
### Transfrom from character to numeric
dfP$CityL <- as.numeric(as.character(dfP$CityL))
dfP$CityR <- as.numeric(as.character(dfP$CityR))
dfP$DptoL <- as.numeric(as.character(dfP$DptoL))
dfP$DptoR <- as.numeric(as.character(dfP$DptoR))
dfP$ChainL <- as.numeric(as.character(dfP$ChainL))
dfP$ChainR <- as.numeric(as.character(dfP$ChainR))
dfP$SuperL <- as.numeric(as.character(dfP$SuperL))
dfP$SuperR <- as.numeric(as.character(dfP$SuperR))
### Create new city pair variable
dfP$maxC <- ifelse(dfP$CityL > dfP$CityR, dfP$CityL, dfP$CityR)
dfP$minC <- ifelse(dfP$CityL > dfP$CityR, dfP$CityR, dfP$CityL)
dfP$CityPair <- ((dfP$maxC * (dfP$maxC + 1)) /2) + dfP$minC
dfP$maxC <- dfP$minC <- NULL
### Create new depto pair variable
dfP$maxD <- ifelse(dfP$DptoL > dfP$DptoR, dfP$DptoL, dfP$DptoR)
dfP$minD <- ifelse(dfP$DptoL > dfP$DptoR, dfP$DptoR, dfP$DptoL)
dfP$DeptoPair <- ((dfP$maxD * (dfP$maxD + 1)) /2) + dfP$minD
dfP$maxD <- dfP$minD <- NULL
### Create new super pair variable
dfP$maxD <- ifelse(dfP$SuperL > dfP$SuperR, dfP$SuperL, dfP$SuperR)
dfP$minD <- ifelse(dfP$SuperL > dfP$SuperR, dfP$SuperR, dfP$SuperL)
dfP$SuperPair <- ((dfP$maxD * (dfP$maxD + 1)) /2) + dfP$minD
dfP$maxD <- dfP$minD <- NULL
### Create new chain pair variable
dfP$maxD <- ifelse(dfP$ChainL > dfP$ChainR, dfP$ChainL, dfP$ChainR)
dfP$minD <- ifelse(dfP$ChainL > dfP$ChainR, dfP$ChainR, dfP$ChainL)
dfP$ChainPair <- ((dfP$maxD * (dfP$maxD + 1)) /2) + dfP$minD
dfP$maxD <- dfP$minD <- NULL
### Continue as.factor()
dfP$CityL <- as.factor(dfP$CityL)
dfP$CityR <- as.factor(dfP$CityR)
dfP$DptoL <- as.factor(dfP$DptoL)
dfP$DptoR <- as.factor(dfP$DptoR)
dfP$ChainL <- as.factor(dfP$ChainL)
dfP$ChainR <- as.factor(dfP$ChainR)
dfP$SuperL <- as.factor(dfP$SuperL)
dfP$SuperR <- as.factor(dfP$SuperR)
dfP$CityPair <- as.factor(dfP$CityPair)
dfP$DeptoPair <- as.factor(dfP$DeptoPair)
dfP$SuperPair <- as.factor(dfP$SuperPair)
dfP$ChainPair <- as.factor(dfP$ChainPair)

str(dfP)

gc()

saveRDS(dfP, file = "/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff.rds")


###########################################
############# Base regression #############

dfP$DptoL <- dfP$DptoR <- NULL
gc()

#### Engel and Rogers

# Base regression, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain
                     | Product + Time | 0 | CityR + CityL + Time, dfP))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance 
                     | Product + SuperL + SuperR + Time | 0 | CityR + CityL + Time, dfP))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (Super FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects again, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance 
                     | Product + SuperPair + Time | 0 | CityR + CityL + Time, dfP))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (Super pair FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


#### Regresion nuestra #### 

# Base regression, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain
                     | Product + Time | 0 | CityR + CityL + Time, dfP))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression NUESTRA (clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance
                     | Product + SuperL + SuperR + Time | 0 | CityR + CityL + Time, dfP))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression NUESTRA (Super FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects again, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety
                     | Product + SuperPair + Time | 0 | CityR + CityL + Time, dfP))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (Super pair FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


## Demanding regresions!

# Absorbing Product * Super, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity 
                     | ChainPair * Product + Time | 0 | CityR + CityL + Time, dfP))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (super * product, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


# Absorbing Product * Super, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety
                     | ChainPair * Product + Time | 0 | CityR + CityL + Time, dfP))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (super * product, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()



#########################################
############# 30 kilometers #############


dfP30 <- dfP[dfP$Distance <= log(31),] # 30kilometers
rm(dfP)

#### Engel and Rogers

# Base regression, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain
                     | Product + Time | 0 | CityR + CityL + Time, dfP30))
sink("salidaV40-2019.txt", append = T)
print("---------- 30 kilometers distance  -----------")
print("---------- Base Regression ER (clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance
                     | Product + SuperL + SuperR + Time | 0 | CityR + CityL + Time, dfP30))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (Super FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects again, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain
                     | Product + SuperPair + Time | 0 | CityR + CityL + Time, dfP30))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (Super pair FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Product * Super, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity
                     | ChainPair * Product + Time | 0 | CityR + CityL + Time, dfP30))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (super * product, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


#### Regresion nuestra #### 

# Base regression, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain
                     | Product + Time | 0 | CityR + CityL + Time, dfP30))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression NUESTRA (clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance
                     | Product + SuperL + SuperR + Time | 0 | CityR + CityL + Time, dfP30))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression NUESTRA (Super FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects again, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety
                     | Product + SuperPair + Time | 0 | CityR + CityL + Time, dfP30))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (Super pair FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Product * Super, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity
                     | ChainPair * Product + Time | 0 | CityR + CityL + Time, dfP30))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (super * product, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()  
  
rm(dfP30)


######################################
############# Montevideo #############

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff.rds") #ClusterUy
dfPM <- dfP[dfP$CityL == 30 & dfP$CityR ==30,] # 30kilometers
rm(dfP)
gc()


#### Engel and Rogers

# Base regression, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance + SameChain
                     | Product + Time | 0 | SuperL + SuperR + Time, dfPM))
sink("salidaV40-2019.txt", append = T)
print("---------- Montevideo  -----------")
print("---------- Base Regression ER (clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance 
                     | Product + SuperL + SuperR + Time | 0 | SuperL + SuperR + Time, dfPM))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (Super FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects again, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance
                     | Product + SuperPair + Time | 0 | SuperL + SuperR + Time, dfPM))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (Super pair FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Product * Super, clustered City + Time
reg1 <- summary(felm(DifPrice ~ Distance + SameChain
                     | ChainPair * Product + Time | 0 | SuperL + SuperR + Time, dfPM))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (super * product, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


#### Regresion nuestra #### 

# Base regression, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + SameChain
                     | Product + Time | 0 | SuperL + SuperR + Time, dfPM))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression NUESTRA (clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance
                     | Product + SuperL + SuperR + Time | 0 | SuperL + SuperR + Time, dfPM))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression NUESTRA (Super FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing store fixed effects again, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety
                     | Product + SuperPair + Time | 0 | SuperL + SuperR + Time, dfPM))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (Super pair FE, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Product * Super, clustered City + Time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance 
                     | ChainPair * Product + Time | 0 | SuperL + SuperR + Time, dfPM))
sink("salidaV40-2019.txt", append = T)
print("---------- Base Regression ER (super * product, clustered City + time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()  

rm(dfPM)

rm(list=ls())


############### End of script ###############
#############################################