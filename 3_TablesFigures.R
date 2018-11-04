gc() #erase garbage

################################################
############## Tables and Figures ##############
################################################

# dbf original database
# dfP price difference database


### Load databases

dbf <-  get(load("/clusteruy/home/leandroz/Bases/Convergence/2018BaseResiduals.Rdata")) #ClusterUy
dfP <-  get(load("/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff.Rdata")) #ClusterUy

dim(dfP)
dim(dbf)

################################################
#############  Tabla 1 (antes 2) ###############


#### Original database ####

## Number of observations by products, fivenums & sd

numb <- by(exp(dbf$moda / 100), dbf$Product, length) # Numeber of observations by product
five1 <- by(exp(dbf$moda / 100), dbf$Product, fivenum) # fivenums
sd1 <- by(exp(dbf$moda / 100), dbf$Product, sd) # sd

sink("salida1.txt", append = T)
print("### Number of observations ###")
print(numb, include.rownames=F)
print("### Five numbers ###")
print(five1, include.rownames=F)
print("### Standard deviation ###")
print(sd1, include.rownames=F)
sink()

rm (numb, five1, sd1)
gc()

## Sample start

start <- by(dbf$Time, dbf$Product, min) # Numeber of observations by product
sink("salida1.txt", append = T)
print("### Sample start ###")
print(start, include.rownames=F)
sink()
gc()

## Number of supermarkets

sup <- by(dbf$Super, dbf$Product, unique)# / 386 * 100) ## share of supermarkets by products 


sink("salida1.txt", append = T)
print("### Number of supers by product ###")
sink()



#### Price difference database ####

## Number of observations by products, fivenums & sd

numb <- by(dfP$DifPrice, dfP$Product, length) # Numeber of observations by product
five1 <- by(dfP$DifPrice, dfP$Product, fivenum) # fivenums
sd1 <- by(dfP$DifPrice, dfP$Product, sd) # sd 

sink("salida1.txt", append = T)
print("### Number of observations: price difference ###")
print(numb, include.rownames=F)
print("### Five numbers: price difference ###")
print(five1, include.rownames=F)
print("### Standard deviation: price difference ###")
print(sd1, include.rownames=F)
sink()

rm (numb, five1, sd1)
gc()

## Number of zeroes in price diff database

h1 = function (x) (length(which(x == 0))/(length(x))*100) ## Total equal zero
z1 <- tapply(dfP$DifPrice, dfP$Product, h1)

sink("salida1.txt", append = T)
print("### Number of zeros: price difference ###")
print(z1, include.rownames=F)
sink()
gc()


################################################ 
#############  Tabla 3 (antes 4) ###############


## Number of observations, fivenums & sd

numb <- length(dfP$DifPrice) # Number of observations
five1 <- fivenum(dfP$DifPrice) # fivenums
sd1 <- sd(dfP$DifPrice) # sd
zros = length(which(dfP$DifPrice == 0))/(length(dfP$DifPrice))*100 ## Total equal zero

sink("salidaV36.txt", append = T)
print("---------------TOTAL-----------------")
print(numb, include.rownames=F)
print(five1, include.rownames=F)
print(sd1, include.rownames=F)
print(zros, include.rownames=F)
sink()
gc()

## Number of observations, fivenums & sd (between and within cities)

# Between cities
numb <- length(dfP[dfP$DifCity == 1,]$DifPrice) # Numeber of observations
five1 <- fivenum(dfP[dfP$DifCity == 1,]$DifPrice) # fivenums
sd1 <- sd(dfP[dfP$DifCity == 1,]$DifPrice) # sd
zros = length(which(dfP[dfP$DifCity == 1,]$DifPrice == 0))/
  (length(dfP[dfP$DifCity == 1,]$DifPrice))*100 # Between cities 

sink("salidaV36.txt", append = T)
print("---------------Between cities----------------")
print(numb, include.rownames=F)
print(five1, include.rownames=F)
print(sd1, include.rownames=F)
print(zros, include.rownames=F)
sink()
gc()

# Within cities
numb <- length(dfP[dfP$DifCity == 0,]$DifPrice) # Numeber of observations
five1 <- fivenum(dfP[dfP$DifCity == 0,]$DifPrice) # fivenums
sd1 <- sd(dfP[dfP$DifCity == 0,]$DifPrice) # sd
zros = length(which(dfP[dfP$DifCity == 0,]$DifPrice == 0))/
  (length(dfP[dfP$DifCity == 0,]$DifPrice))*100 # Within cities

sink("salidaV36.txt", append = T)
print("---------------Within cities----------------")
print(numb, include.rownames=F)
print(five1, include.rownames=F)
print(sd1, include.rownames=F)
print(zros, include.rownames=F)
sink()
gc()

# Within Montevideo
numb <- length(dfP[dfP$CityL == 30 & dfP$CityR == 30,]$DifPrice) # Numeber of observations
five1 <- fivenum(dfP[dfP$CityL == 30 & dfP$CityR == 30,]$DifPrice) # fivenums
sd1 <- sd(dfP[dfP$CityL == 30 & dfP$CityR == 30,]$DifPrice) # sd
zros = length(which(dfP[dfP$CityL == 30 & dfP$CityR == 30,]$DifPrice == 0))/
  (length(dfP[dfP$CityL == 30 & dfP$CityR == 30,]$DifPrice))*100 # Within cities

sink("salidaV36.txt", append = T)
print("---------------Within Montevideo----------------")
print(numb, include.rownames=F)
print(five1, include.rownames=F)
print(sd1, include.rownames=F)
print(zros, include.rownames=F)
sink()
gc()

# Excluded Montevideo
numb <- length(dfP[dfP$CityL != 30 & dfP$CityR != 30,]$DifPrice) # Numeber of observations
five1 <- fivenum(dfP[dfP$CityL != 30 & dfP$CityR != 30,]$DifPrice) # fivenums
sd1 <- sd(dfP[dfP$CityL != 30 & dfP$CityR != 30,]$DifPrice) # sd
zros = length(which(dfP[dfP$CityL != 30 & dfP$CityR != 30,]$DifPrice == 0))/
  (length(dfP[dfP$CityL != 30 & dfP$CityR != 30,]$DifPrice))*100 # Within cities

sink("salidaV36.txt", append = T)
print("---------------Excluded Montevideo----------------")
print(numb, include.rownames=F)
print(five1, include.rownames=F)
print(sd1, include.rownames=F)
print(zros, include.rownames=F)
sink()
gc()

# Montevideo - Others
dfP$MontOthers <- ifelse(dfP$CityL == 30 | dfP$CityR == 30, 1, 0)
numb <- length(dfP[dfP$MontOthers == 1 & dfP$DifCity == 1,]$DifPrice) # Numeber of observations
five1 <- fivenum(dfP[dfP$MontOthers == 1 & dfP$DifCity == 1,]$DifPrice) # fivenums
sd1 <- sd(dfP[dfP$MontOthers == 1 & dfP$DifCity == 1,]$DifPrice) # sd
zros = length(which(dfP[dfP$MontOthers == 1 & dfP$DifCity == 1,]$DifPrice == 0))/
  (length(dfP[dfP$MontOthers == 1 & dfP$DifCity == 1,]$DifPrice))*100 # Within cities

sink("salidaV36.txt", append = T)
print("--------------- Montevideo - Others----------------")
print(numb, include.rownames=F)
print(five1, include.rownames=F)
print(sd1, include.rownames=F)
print(zros, include.rownames=F)
sink()
gc()


## Number of observations, fivenums & sd (competition & variety)

# Competition
for (c in unique(dfP$DComp))
{
  numb <- length(dfP[dfP$DComp == c,]$DifPrice) # Numeber of observations
  five1 <- fivenum(dfP[dfP$DComp == c,]$DifPrice) # fivenums
  sd1 <- sd(dfP[dfP$DComp == c,]$DifPrice) # sd
  zros = length(which(dfP[dfP$DComp == c,]$DifPrice == 0))/
    (length(dfP[dfP$DComp == c,]$DifPrice))*100 # 
  
  sink("salidaV36.txt", append = T)
  print(paste0("--------------- Diff Competition = ", c, "------------"))
  print(numb, include.rownames=F)
  print(five1, include.rownames=F)
  print(sd1, include.rownames=F)
  print(zros, include.rownames=F)
  sink()
  gc()
}

# Variety
for (c in unique(dfP$DVariety))
{
  numb <- length(dfP[dfP$DVariety == c,]$DifPrice) # Numeber of observations
  five1 <- fivenum(dfP[dfP$DVariety == c,]$DifPrice) # fivenums
  sd1 <- sd(dfP[dfP$DVariety == c,]$DifPrice) # sd
  zros = length(which(dfP[dfP$DVariety == c,]$DifPrice == 0))/
    (length(dfP[dfP$DVariety == c,]$DifPrice))*100 # 
  
  sink("salidaV36.txt", append = T)
  print(paste0("--------------- Diff Variety = ", c, "------------"))
  print(numb, include.rownames=F)
  print(five1, include.rownames=F)
  print(sd1, include.rownames=F)
  print(zros, include.rownames=F)
  sink()
  gc()
}

rm(numb, five1, sd1, zros)



################################################
###### Figure 2: Observations by distance ######

h1 = hist(exp(dfP$Distance)-1) # Number of observations by kilometer
h1$density = h1$counts/sum(h1$counts)*100
h2 = hist(exp(dfP[dfP$DifCity == 0,]$Distance)-1) # Number of observations by kilometer inside cities
h2$density = h2$counts/sum(h2$counts)*100
h3 = hist(exp(dfP[dfP$DifCity == 1,]$Distance)-1) # Number of observations by kilometer between cities
h3$density = h3$counts/sum(h3$counts)*100


pdf("/clusteruy/home/leandroz/Figuras/plot2.pdf", width=11.69, height=5)
par(mfrow=c(1,3))
plot(h1,freq=FALSE, main = "All Observations", ylim = c(0,50),
     xlab = "Kilometers", ylab = "Relative Frequency", col = "gray71")
plot(h2,freq=FALSE, main = "Within Cities", ylim = c(0,15),
     xlab = "Kilometers", ylab = "Relative Frequency", col = "gray71")
plot(h3,freq=FALSE, main = "Between Cities", ylim = c(0,20),
     xlab = "Kilometers", ylab = "Relative Frequency", col = "gray71")
dev.off()


################################################
#############  Figure 3: # zeroes ##############


h1 = hist(dfP$DifPrice)
h1$density = h1$counts/sum(h1$counts)*100 ## All
h2 = hist(dfP[dfP$DifCity == 0 & dfP$Distance <= log(31),]$DifPrice) ## Distribution within up to 40 km
h2$density = h2$counts/sum(h2$counts)*100
h3 = hist(dfP[dfP$DifCity == 1 & dfP$Distance <= log(31),]$DifPrice) ## Distribution between up to 40 km
h3$density = h3$counts/sum(h3$counts)*100

pdf("/clusteruy/home/leandroz/Figuras/plot1.pdf", width=11.69, height=5)
#pdf(file = "/home/lzipitria/plot1.pdf", width=11.69, height=5)
par(mfrow=c(1,3))
plot(h1,freq=FALSE, main = "All", ylim = c(0,60), xlim = c(0, 60),
     xlab = "Price Differences", ylab = "Relative Frequency", col = "gray71")
plot(h2,freq=FALSE, main = "Within Cities (up to 30km)", ylim = c(0,60), xlim = c(0, 60),
     xlab = "Price Differences", ylab = "Relative Frequency", col = "gray71")
plot(h3,freq=FALSE, main = "Between Cities (up to 30 km)", ylim = c(0,60), xlim = c(0, 60),
     xlab = "Price Differences", ylab = "Relative Frequency", col = "gray71")
dev.off()



#####################################################
####  Figure 4: Price difference and competition #### 


h1 = hist(dfP[dfP$DComp == 0 & dfP$DVariety == 0,]$DifPrice)
h1$density = h1$counts/sum(h1$counts)*100
h2 = hist(dfP[dfP$DComp == 2 & dfP$DVariety == 0,]$DifPrice)
h2$density = h2$counts/sum(h2$counts)*100
h3 = hist(dfP[dfP$DComp == 0 & dfP$DVariety == 2,]$DifPrice)
h3$density = h3$counts/sum(h3$counts)*100

pdf("/clusteruy/home/leandroz/Figuras/plot3a.pdf", width=8, height=5) #revisar
par(mfrow=c(1,3))
plot(h1,freq=FALSE, main = "Same Varieties", ylim = c(0,70), xlim = c(0, 50),
     xlab = "Price Differences", ylab = "Relative Frequency", col = "gray71")
plot(h2,freq=FALSE, main = "Two Competitors", ylim = c(0,70), xlim = c(0,50),
     xlab = "Price Differences", ylab = "Relative Frequency", col = "gray71")
plot(h3,freq=FALSE, main = "Two Varieties",  ylim = c(0,70), xlim = c(0, 50),
     xlab = "Price Differences", ylab = "Relative Frequency", col = "gray71")
# col = rgb(0,0,1,1/4)
dev.off()


## Call next script

source("/clusteruy/home/leandroz/4_Regressions.R")