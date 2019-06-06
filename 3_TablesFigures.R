unlink(".RData") 
gc() #erase garbage

################################################
############## Tables and Figures ##############
################################################

# dbf original database
# dfP price difference database

library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy
#library(data.table)

### Load databases

dbf <-  readRDS("/clusteruy/home/leandroz/Bases/Convergence/2018BaseResiduals.rds") #ClusterUy
dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff.rds") #ClusterUy
#dbf <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/Viejas/2018BaseResiduals.rds")
#dfP <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/less200metters.rds")
dbf <- readRDS("c://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/Viejas/2018BaseResiduals.rds")
dfP <- readRDS("c://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/less200metters.rds")

dim(dfP)
dim(dbf)

################################################
#############  Table 1 (antes 2) ###############


#### ------ Original Price Database ------------

## Number of observations by products, fivenums, sd, sample start, and number of supermarkets
dbf$origMode <- exp(dbf$moda / 100) # for speeding calculations
# Five numbers
fiveN <- setDT(dbf)[, fivenum(origMode), (Product)] # five numbers
fiveN$vars <- rep(c("Minimum", "notU", "Median", "notU2", "Maximum"), len = nrow(fiveN))
fiveN <- dcast(data=fiveN, formula = Product ~ vars, value.var = "V1")
fiveN$notU <- fiveN$notU2 <- NULL
# Number of observations by product
nobs <-  setDT(dbf)[, .N, by = Product] # number of observations
# Standard deviatin
estd <- setDT(dbf)[, sd(origMode), (Product)] # standard deviations
setnames(estd, old = "V1", new = "Standard-Deviation")
# Number of supermarkets
nsup <- length(unique(dbf$Super))
sup <- setDT(dbf)[,length(unique(Super))/nsup*100, (Product)]  # maximum number of supermarkets by product
setnames(sup, old = "V1", new = "Share-stores")
# Start date for each product
sstart <- setDT(dbf)[,min(Time), (Product)]
setnames(sstart, old = "V1", new = "TStart")
# merge all information into one database (fiveN, nobs, sup, estd, sstart)
info <- merge(merge(merge(merge(fiveN, nobs, by = "Product"), estd, by = "Product"), sstart, by ="Product"), sup, by="Product")
info <- info[,c("Product","Minimum", "Median","Maximum","Standard-Deviation", "N", "Share-stores", "TStart")]
write.csv(info, "/clusteruy/home/leandroz/Bases/Convergence/InfoOriginDatabase.csv", row.names = F)
# Delete and erase garbage
rm (fiveN,nobs,estd,info,sstart,sup,nsup)
gc()



#### ------- Price difference database ----------

## Number of observations by products, fivenums & sd
# Five numbers
fiveN <- setDT(dfP)[, fivenum(DifPrice), (Product)] # five numbers
fiveN$vars <- rep(c("Minimum", "notU", "Median", "notU2", "Maximum"), len = nrow(fiveN))
fiveN <- dcast(data=fiveN, formula = Product ~ vars, value.var = "V1")
fiveN$notU <- fiveN$notU2 <- NULL
# Number of observations by product
nobs <-  setDT(dfP)[, .N, by = Product] # number of observations
# Standard deviatin
estd <- setDT(dfP)[, sd(DifPrice), (Product)] # standard deviations
setnames(estd, old = "V1", new = "Standard-Deviation")
# Number of exact zeroes
h1 = function (x) (length(which(x == 0))/(length(x))*100) ## Total equal zero
z1 <- setDT(dfP)[, h1(DifPrice), (Product)]
setnames(z1, old = "V1", new = "Exact-zeroes") 
# merge all information into one database (fiveN, nobs, estd, z1)
info <- merge(merge(merge(fiveN, nobs, by = "Product"), estd, by = "Product"), z1, by ="Product")
info <- info[,c("Product","Minimum", "Median","Maximum","Standard-Deviation", "N", "Exact-zeroes")]
write.csv(info, "/clusteruy/home/leandroz/Bases/Convergence/InfoPriceDiff.csv", row.names = F)
# Erase information and collect garbage
rm (nobs,fiveN,estd,z1,info)
gc()



############################################## 
#------------  Tables 3 and 4-----------------

## Cross tabulation

cross1 <- table(dbf$competition, dbf$variety) #   el primero es fila, segundo columna
cross2 <- table(dfP$Dcomp, dfP$DVariety) #  el primero es fila, segundo columna
prop.table(aa)*100

sink("salidaV40-2019.txt", append = T)
print("---------------Cross Tab: original database -----------------")
print(prop.table(cross1)*100, include.rownames=F)
print("---------------Cross Tab: Price difference database -----------------")
print(prop.table(cross2)*100, include.rownames=F)
sink()
gc()


## Number of observations, fivenums & sd

numb <- length(dfP$DifPrice) # Number of observations
five1 <- fivenum(dfP$DifPrice) # fivenums
sd1 <- sd(dfP$DifPrice) # sd
zros = length(which(dfP$DifPrice == 0))/(length(dfP$DifPrice))*100 ## Total equal zero

sink("salidaV40-2019.txt", append = T)
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

sink("salidaV40-2019.txt", append = T)
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

sink("salidaV40-2019.txt", append = T)
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

sink("salidaV40-2019.txt", append = T)
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

sink("salidaV40-2019.txt", append = T)
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

sink("salidaV40-2019.txt", append = T)
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
  
  sink("salidaV40-2019.txt", append = T)
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
  
  sink("salidaV40-2019.txt", append = T)
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

