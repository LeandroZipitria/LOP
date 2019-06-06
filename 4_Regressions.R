unlink(".RData")
# load packages
library("lfe", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## Cluster
library("dplyr", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## Cluster

dfP1 <-  get(load("/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff.Rdata")) #ClusterUy
date <- read.csv("/clusteruy/home/leandroz/Bases/Border/Date.csv")

str(dfP)

Y2007 <- c(1:9)
Y2008 <- c(10:21)
Y2009 <- c(22:33)
Y2010 <- c(34:45)
Y2011 <- c(46:57)
Y2012 <- c(58:69)
Y2013 <- c(70:81)

dfP1$Year <- ifelse(dfP1$Time %in% Y2007, 2007,
                   ifelse(dfP1$Time %in% Y2008, 2008,
                          ifelse(dfP1$Time %in% Y2009, 2009,
                                 ifelse(dfP1$Time %in% Y2010, 2010,
                                        ifelse(dfP1$Time %in% Y2011, 2011,
                                               ifelse(dfP1$Time %in% Y2012, 2012,
                                                      ifelse(dfP1$Time %in% Y2013, 2013,2014)))))))


rm(Y2007,Y2008,Y2009,Y2010,Y2011,Y2012,Y2013)

### Transform variables using as.factor()
dfP1$CityL <- as.factor(dfP1$CityL)
dfP1$CityR <- as.factor(dfP1$CityR)
dfP1$SameChain <- ifelse(dfP1$ChainR == dfP1$ChainL, 1,0)
dfP1$ChainL <- as.factor(dfP1$ChainL)
dfP1$ChainR <- as.factor(dfP1$ChainR)
dfP1$SuperL <- as.factor(dfP1$SuperL)
dfP1$SuperR <- as.factor(dfP1$SuperR)
dfP1$DptoL <- as.factor(dfP1$DptoL)
dfP1$DptoR <- as.factor(dfP1$DptoR)
dfP$Time <- as.factor(dfP1$Time)
dfP1$Product <- as.factor(dfP1$Product)

str(dfP1)

gc()


### Loop

#dfP1 <-  get(load("/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff2.Rdata")) #ClusterUy

for (i in unique(dfP1$Year)) {

  dfP <- dfP1[dfP1$Year == i,]
#  rm(dfP1)
  
  ## Cross table
  aa <- table(dfP$DVariety, dfP$DComp) #  el primero es fila, segundo columna
  z <- round(prop.table(aa), digits = 5) * 100
  N <- nrow(dfP)
  
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------------Cross Table ----------------")
  print(z, include.rownames=F)
  print("---------------Number of observations ----------------")
  print(N, include.rownames=F)
  sink()
  gc()
  rm(aa, z, N)
  
  ###########################################
  ############# Base regression #############

  #### Engel and Rogers

  # Absorbing City (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | CityR + CityL | 0 | Product + Time, dfP))

  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Base Regression ER (city, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()

  # Absorbing City (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | CityR + CityL | 0 | CityR + CityL + Time, dfP))

  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- Base Regression ER (city, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()

  # Absorbing City (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | CityR + CityL | 0 | Product + CityR + CityL + Time, dfP))

  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- Base Regression ER (city, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Product * Super (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Base Regression ER (super * product, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Super (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | CityR + CityL + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- Base Regression ER (super * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Super (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | Product + CityR + CityL + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- Base Regression ER (super * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Product * Chain (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Base Regression ER (chain * product, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Chain (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | CityR + CityL + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- Base Regression ER (chain * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Chain (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | Product + CityR + CityL + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- Base Regression ER (chain * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############

  # Absorbing Super (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Base Regression ER (super, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | CityR + CityL + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- Base Regression ER (super, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | Product + CityR + CityL + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- Base Regression ER (super, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()


  #### Regresion nuestra #### 

  # Absorbing City, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Product + Time
                         | CityR + CityL | 0 | Product + Time, dfP))

  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (city, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()

  # Absorbing City, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                         | CityR + CityL | 0 | CityR + CityL + Time, dfP))

  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (city, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing City, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | CityR + CityL | 0 | CityR + CityL + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (city, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()

  ###############
  
  # Absorbing Super, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Product + Time
                       | SuperR + SuperL | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (super, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | SuperR + SuperL | 0 | CityR + CityL + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (super, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | SuperR + SuperL | 0 | CityR + CityL + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (super, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############

  # Absorbing Super * Product, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (SuperR + SuperL) * Product | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (super * product, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * Product, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (SuperR + SuperL) * Product | 0 | CityR + CityL + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (super * product, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * Product, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (SuperR + SuperL) * Product | 0 | CityR + CityL + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (super * product, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############

  # Absorbing Chain * Product, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (ChainR + ChainL) * Product | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (chain * product, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * Product, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (ChainR + ChainL) * Product | 0 | CityR + CityL + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (chain * product, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * Product, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (ChainR + ChainL) * Product | 0 | CityR + CityL + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Base Regression Nuestra (chain * product, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  

  #########################################################
  ############# Base regression: Departamento #############

  dfP$BorderDepto <- ifelse(dfP$DptoL != dfP$DptoR, 1, 0)



  #### Engel and Rogers #### 

  # Absorbing Depto, clustered Products + time
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Product + Time 
                       | DptoL + DptoR | 0 | Product + Time, dfP))

  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (depto, clustered product + time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Depto, clustered Depto + time
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Product + Time 
                       | DptoL + DptoR | 0 | DptoL + DptoR + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (depto, clustered depto + time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Depto, clustered Depto + time + product
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Product + Time 
                       | DptoL + DptoR | 0 | DptoL + DptoR + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (depto, clustered depto + time + product) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Super, clustered Products + time
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (Super, clustered product + time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered Depto + time
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | DptoL + DptoR + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (Super, clustered depto + time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered Depto + time + product
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | DptoL + DptoR + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (Super, clustered depto + time + product) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Super * product, clustered Products + time
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (Super * product, clustered product + time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * product, clustered Depto + time
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | DptoL + DptoR + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (Super * product, clustered depto + time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * product, clustered Depto + time + product
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | DptoL + DptoR + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (Super * product, clustered depto + time + product) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Chain * product, clustered Products + time
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (Chain * product, clustered product + time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * product, clustered Depto + time
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | DptoL + DptoR + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (Chain * product, clustered depto + time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * product, clustered Depto + time + product
  reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | DptoL + DptoR + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ Depto Regression ER (Chain * product, clustered depto + time + product) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  

  #### Regresion nuestra  ####

  # Absorbing depto, clustered Products + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Product + Time
                       | DptoL + DptoR | 0 | Product + Time, dfP))

  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (depto, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()

  # Absorbing depto, clustered depto + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Product + Time
                       | DptoL + DptoR | 0 | DptoL + DptoR + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (depto, clustered depto + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing depto, clustered depto + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Product + Time
                       | DptoL + DptoR | 0 | DptoL + DptoR + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (depto, clustered depto + time + product) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  
  # Absorbing super, clustered Products + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Product + Time
                       | SuperL + SuperR | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (Super, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered depto + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Product + Time
                       | SuperL + SuperR | 0 | DptoL + DptoR + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (Super, clustered depto + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered depto + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Product + Time
                       | SuperL + SuperR | 0 | DptoL + DptoR + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (Super, clustered depto + time + product) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing super * product, clustered Products + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Time
                       | (SuperL + SuperR) * Product | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (Super * product, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * product, clustered depto + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Time
                       | (SuperL + SuperR) * Product | 0 | DptoL + DptoR + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (Super * product, clustered depto + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * product, clustered depto + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Time
                       | (SuperL + SuperR) * Product | 0 | DptoL + DptoR + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (Super, clustered depto + time + product) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  
  # Absorbing chain * product, clustered Products + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Time
                       | (ChainL + ChainR) * Product | 0 | Product + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (Chain * product, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * product, clustered depto + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Time
                       | (ChainL + ChainR) * Product | 0 | DptoL + DptoR + Time, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (Chain * product, clustered depto + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * product, clustered depto + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + SameChain + Time
                       | (ChainL + ChainR) * Product | 0 | DptoL + DptoR + Time + Product, dfP))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- Depto Regression Nuestra (Chain, clustered depto + time + product) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  

  #########################################
  ############# 20 kilometers #############


  dfP20 <- dfP[dfP$Distance <= log(21),] # 20kilometers


  #### Engel and Rogers
  
  # Absorbing City (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | CityR + CityL | 0 | Product + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ 20 km Base Regression ER (city, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing City (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | CityR + CityL | 0 | CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 20 km Base Regression ER (city, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing City (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | CityR + CityL | 0 | Product + CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 20 km Base Regression ER (city, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Product * Super (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | Product + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ 20 km Base Regression ER (super * product, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Super (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 20 km Base Regression ER (super * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Super (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | Product + CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 20 km Base Regression ER (super * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Product * Chain (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | Product + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ 20 km Base Regression ER (chain * product, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Chain (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 20 km Base Regression ER (chain * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Chain (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | Product + CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 20 km Base Regression ER (chain * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Super (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | Product + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ 20 km Base Regression ER (super, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 20 km Base Regression ER (super, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | Product + CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 20 km Base Regression ER (super, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  
  #### Regresion nuestra #### 
  
  # Absorbing City, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Product + Time
                       | CityR + CityL | 0 | Product + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (city, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing City, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | CityR + CityL | 0 | CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (city, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing City, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | CityR + CityL | 0 | CityR + CityL + Time + Product, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (city, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Super, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Product + Time
                       | SuperR + SuperL | 0 | Product + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (super, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | SuperR + SuperL | 0 | CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (super, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | SuperR + SuperL | 0 | CityR + CityL + Time + Product, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (super, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Super * Product, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (SuperR + SuperL) * Product | 0 | Product + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (super * product, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * Product, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (SuperR + SuperL) * Product | 0 | CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20km Base Regression Nuestra (super * product, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * Product, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (SuperR + SuperL) * Product | 0 | CityR + CityL + Time + Product, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (super * product, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Chain * Product, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (ChainR + ChainL) * Product | 0 | Product + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (chain * product, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * Product, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (ChainR + ChainL) * Product | 0 | CityR + CityL + Time, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (chain * product, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * Product, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (ChainR + ChainL) * Product | 0 | CityR + CityL + Time + Product, dfP20))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 20 km Base Regression Nuestra (chain * product, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  
  rm(dfP20)


  #########################################
  ############# 30 kilometers #############



  dfP30 <- dfP[dfP$Distance <= log(31),] # 30kilometers

  #### Engel and Rogers
  
  # Absorbing City (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | CityR + CityL | 0 | Product + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ 30 km Base Regression ER (city, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing City (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | CityR + CityL | 0 | CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 30 km Base Regression ER (city, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing City (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | CityR + CityL | 0 | Product + CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 30 km Base Regression ER (city, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Product * Super (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | Product + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ 30 km Base Regression ER (super * product, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Super (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 30 km Base Regression ER (super * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Super (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (SuperL + SuperR) * Product | 0 | Product + CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 30 km Base Regression ER (super * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Product * Chain (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | Product + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ 30 km Base Regression ER (chain * product, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Chain (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 30 km Base Regression ER (chain * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Product * Chain (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Time 
                       | (ChainL + ChainR) * Product | 0 | Product + CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 30 km Base Regression ER (chain * product, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Super (Gorodnichenko), clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | Product + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("------------ 30 km Base Regression ER (super, clustered product + Time) --------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super (Gorodnichenko), clustered City + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 30 km Base Regression ER (super, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super (Gorodnichenko), clustered City + Time + Product
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain + Product + Time 
                       | SuperL + SuperR | 0 | Product + CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("---------- 30 km Base Regression ER (super, clustered product + City + time) -----------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  
  #### Regresion nuestra #### 
  
  # Absorbing City, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Product + Time
                       | CityR + CityL | 0 | Product + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (city, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing City, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | CityR + CityL | 0 | CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (city, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing City, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | CityR + CityL | 0 | CityR + CityL + Time + Product, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (city, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Super, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Product + Time
                       | SuperR + SuperL | 0 | Product + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (super, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | SuperR + SuperL | 0 | CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (super, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain +Product + Time
                       | SuperR + SuperL | 0 | CityR + CityL + Time + Product, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (super, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Super * Product, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (SuperR + SuperL) * Product | 0 | Product + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (super * product, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * Product, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (SuperR + SuperL) * Product | 0 | CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (super * product, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Super * Product, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (SuperR + SuperL) * Product | 0 | CityR + CityL + Time + Product, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (super * product, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  ###############
  
  # Absorbing Chain * Product, clustered Products + Time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (ChainR + ChainL) * Product | 0 | Product + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (chain * product, clustered product + time) -------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * Product, clustered City + time
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (ChainR + ChainL) * Product | 0 | CityR + CityL + Time, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (chain * product, clustered city + time) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing Chain * Product, clustered City + time + product
  reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + SameChain + Time
                       | (ChainR + ChainL) * Product | 0 | CityR + CityL + Time + Product, dfP30))
  
  sink(paste0("salidaV36RegAnio_",i,".txt"), append = T)
  print("----------- 30 km Base Regression Nuestra (chain * product, clustered city + time + product) ------------")
  print(reg1, include.rownames=F)
  sink()
  rm(reg1)
  gc()
  
  
  rm(dfP30)

  ############### End of script ###############
  #############################################

}

