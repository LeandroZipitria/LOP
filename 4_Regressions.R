unlink(".RData") 
# load packages
library("lfe", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy

dfP <-  get(load("/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff.Rdata")) #ClusterUy

### Transform variables using as.factor()

dfP$CityL <- as.factor(dfP$CityL)
dfP$CityR <- as.factor(dfP$CityR)
dfP$ChainL <- as.factor(dfP$ChainL)
dfP$ChainR <- as.factor(dfP$ChainR)
dfP$SuperL <- as.factor(dfP$SuperL)
dfP$SuperR <- as.factor(dfP$SuperR)
dfP$DptoL <- as.factor(dfP$DptoL)
dfP$DptoR <- as.factor(dfP$DptoR)
dfP$Time <- as.factor(dfP$Time)
dfP$Product <- as.factor(dfP$Product)


###########################################
############# Base regression #############

#### Engel and Rogers

# Absorbing City, clustered Products
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | CityR + CityL | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("------------ Base Regression ER (city, clustered product) --------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing City, clustered Product and time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | CityR + CityL | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- Base Regression ER (city, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Product
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- Base Regression ER (super, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Product and time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- Base Regression ER (super, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


#### Regresion nuestra

# Absorbing City, clustered Products
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | CityR + CityL | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- Base Regression Nuestra (city, clustered product) -------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing City, clustered Products and time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | CityR + CityL | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- Base Regression Nuestra (city, clustered product time) ------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


# Absorbing Super, clustered Products
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | SuperL + SuperR | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- Base Regression Nuestra (super, clustered product) -------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Products and time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- Base Regression Nuestra (super, clustered product time) ------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()




#########################################################
############# Base regression: Departamento #############

dfP$BorderDepto <- ifelse(dfP$DptoL != dfP$DptoR, 1, 0)



#### Engel and Rogers

# Absorbing City, clustered Products
reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + Product + Time | DptoL + DptoR | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("------------ Depto Regression ER (depto, clustered product) --------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing City, clustered Products and time
reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + Product + Time | DptoL + DptoR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- Depto Regression ER (depto, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Products
reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + Product + Time | SuperL + SuperR | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("------------ Depto Regression ER (super, clustered product) --------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Products and time
reg1 <- summary(felm(DifPrice ~ Distance + BorderDepto + Product + Time | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- Depto Regression ER (super, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


#### Regresion nuestra

# Absorbing depto, clustered Products
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + Product + Time
                     | DptoL + DptoR | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- Depto Regression Nuestra (depto, clustered product) -------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing depto, clustered Products and time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + Product + Time 
                       | DptoL + DptoR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- Depto Regression Nuestra (depto, clustered product time) ------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing super, clustered Products
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + Product + Time 
                       | SuperL + SuperR | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- Depto Regression Nuestra (super, clustered product) -------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing super, clustered Products and time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + BorderDepto + Product + Time 
                       | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- Depto Regression Nuestra (super, clustered product time) ------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


#########################################
############# 20 kilometers #############


dfP <- dfP[dfP$Distance <= log(21),] # 20kilometers


#### Engel and Rogers

# Absorbing City, clustered Products
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | CityR + CityL | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("------------ 20km Base Regression ER (city, clustered product) --------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing City, clustered Product and time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | CityR + CityL | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- 20km Base Regression ER (city, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Product
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- 20km Base Regression ER (super, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Product and time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- 20km Base Regression ER (super, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


#### Regresion nuestra

# Absorbing City, clustered Products
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | CityR + CityL | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- 20km Base Regression Nuestra (city, clustered product) -------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing City, clustered Products and time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | CityR + CityL | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- 20km Base Regression Nuestra (city, clustered product time) ------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


# Absorbing Super, clustered Products
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | SuperL + SuperR | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- 20km Base Regression Nuestra (super, clustered product) -------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Products and time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- 20km Base Regression Nuestra (super, clustered product time) ------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

rm(dfP)


#########################################
############# 30 kilometers #############

dfP <- get(load("/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff.Rdata"))

dfP <- dfP[dfP$Distance <= log(31),] # 30kilometers

### Transform variables using as.factor()

dfP$CityL <- as.factor(dfP$CityL)
dfP$CityR <- as.factor(dfP$CityR)
dfP$ChainL <- as.factor(dfP$ChainL)
dfP$ChainR <- as.factor(dfP$ChainR)
dfP$SuperL <- as.factor(dfP$SuperL)
dfP$SuperR <- as.factor(dfP$SuperR)
dfP$DptoL <- as.factor(dfP$DptoL)
dfP$DptoR <- as.factor(dfP$DptoR)
dfP$Time <- as.factor(dfP$Time)
dfP$Product <- as.factor(dfP$Product)


#### Engel and Rogers

# Absorbing City, clustered Products
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | CityR + CityL | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("------------ 20km Base Regression ER (city, clustered product) --------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing City, clustered Product and time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | CityR + CityL | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- 20km Base Regression ER (city, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Product
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- 20km Base Regression ER (super, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Product and time
reg1 <- summary(felm(DifPrice ~ Distance + DifCity + Product + Time | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("---------- 20km Base Regression ER (super, clustered product time) -----------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


#### Regresion nuestra

# Absorbing City, clustered Products
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | CityR + CityL | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- 20km Base Regression Nuestra (city, clustered product) -------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing City, clustered Products and time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | CityR + CityL | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- 20km Base Regression Nuestra (city, clustered product time) ------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()


# Absorbing Super, clustered Products
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | SuperL + SuperR | 0 | Product, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- 20km Base Regression Nuestra (super, clustered product) -------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

# Absorbing Super, clustered Products and time
reg1 <- summary(felm(DifPrice ~ DComp + DVariety + Distance + DifCity + Product + Time 
                       | SuperL + SuperR | 0 | Product + Time, dfP))

sink("salidaV36Reg.txt", append = T)
print("----------- 20km Base Regression Nuestra (super, clustered product time) ------------")
print(reg1, include.rownames=F)
sink()
rm(reg1)
gc()

############### End of script ###############
#############################################

