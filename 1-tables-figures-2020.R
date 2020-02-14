unlink(".RData")

# ----------------------------------------------------------------------------
# ---------------------------- Tables and Figures ----------------------------
# ----------------------------------------------------------------------------

# dbf original database
# dfP price difference database

library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy
#library(data.table)

### Load databases

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/2020-PriceDiff-final.rds") #ClusterUy
dbf <- read.csv("/clusteruy/home/leandroz/Bases/Border/2020-dbase-levels-final.csv")
#dbf <- read.csv("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-levels-final.csv")
gc()




#### ----------------------------- TABLES  -----------------------------


## --------- Table 1 ---------

# share of varieties in price database
a <- (table(dbf$Variety)) / (nrow(dbf)) * 100
a
sink("salidaV41-2020.txt", append = T)
print("---------------------------- Table 1 --------------------------------")
print("--------------- share of varieties in price database ----------------")
print(a)
sink()

# share of varieties in price difference database
a <- (table(dfP$DVar)) / (nrow(dfP)) * 100
a
sink("salidaV41-2020.txt", append = T)
print("------------------------------- Table 1 ----------------------------------------")
print("--------------- share of varieties in price difference database ----------------")
print(a)
sink()

gc()

## --------- Table 2 ---------

#dfP <-  readRDS("c://Users/leandro/Desktop/BasesBorder/panchos_PriceDiff.rds") #ClusterUy
# Total (column 1)
a <- min(dfP$Distance)
b <- median(exp(dfP$Distance-1))
c <- max(exp(dfP$Distance-1))

sink("salidaV41-2020.txt", append = T)
print("---------------------------- Table 2 --------------------------------")
print("--------------- TOTAL ----------------")
print(a)
print(b)
print(c)
sink()

# Within cities (column 2)
a <- min(dfP[dfP$DifCity ==0,]$Distance)
b <- median(dfP[dfP$DifCity ==0,]$Distance)
c <- max(dfP[dfP$DifCity ==0,]$Distance)

sink("salidaV41-2020.txt", append = T)
print("---------------------------- Table 2 --------------------------------")
print("--------------- Within city ----------------")
print(a)
print(b)
print(c)
sink()

# Between cities (column 3)
a <- min(dfP[dfP$DifCity ==1,]$Distance)
b <- median(dfP[dfP$DifCity ==1,]$Distance)
c <- max(dfP[dfP$DifCity ==1,]$Distance)

sink("salidaV41-2020.txt", append = T)
print("---------------------------- Table 2 --------------------------------")
print("--------------- Between city ----------------")
print(a)
print(b)
print(c)
sink()

gc()

## --------- Table 3 ---------


# All observations (First row)
numb <- length(dfP$DifPrice) # Number of observations
five1 <- fivenum(dfP$DifPrice) # fivenums
sd1 <- sd(dfP$DifPrice) # sd
zros = length(which(dfP$DifPrice == 0))/
  (length(dfP$DifPrice))*100 # Number of exact zeroes 

sink("salidaV41-2020.txt", append = T)
print("---------------------------- Table 3 --------------------------------")
print("---------------Total----------------")
print(numb, include.rownames=F)
print(five1, include.rownames=F)
print(sd1, include.rownames=F)
print(zros, include.rownames=F)
sink()
gc()


# Between cities (second row)
numb <- length(dfP[dfP$DifCity == 1,]$DifPrice) # Number of observations
five1 <- fivenum(dfP[dfP$DifCity == 1,]$DifPrice) # fivenums
sd1 <- sd(dfP[dfP$DifCity == 1,]$DifPrice) # sd
zros = length(which(dfP[dfP$DifCity == 1,]$DifPrice == 0))/
  (length(dfP[dfP$DifCity == 1,]$DifPrice))*100 # Number of exact zeroes 

sink("salidaV41-2020.txt", append = T)
print("---------------Between cities----------------")
print(numb, include.rownames=F)
print(five1, include.rownames=F)
print(sd1, include.rownames=F)
print(zros, include.rownames=F)
sink()
gc()

# Within cities (third row)
numb <- length(dfP[dfP$DifCity == 0,]$DifPrice) # Number of observations
five1 <- fivenum(dfP[dfP$DifCity == 0,]$DifPrice) # fivenums
sd1 <- sd(dfP[dfP$DifCity == 0,]$DifPrice) # sd
zros = length(which(dfP[dfP$DifCity == 0,]$DifPrice == 0))/
  (length(dfP[dfP$DifCity == 0,]$DifPrice))*100 # Number of exact zeroes

sink("salidaV41-2020.txt", append = T)
print("---------------Within cities----------------")
print(numb, include.rownames=F)
print(five1, include.rownames=F)
print(sd1, include.rownames=F)
print(zros, include.rownames=F)
sink()
gc()


# Varieties (0: fourth row; 1: fifth row)

val = c(0,1)
for (c in val)
{
  numb <- length(dfP[dfP$DVar == c,]$DifPrice) # Number of observations
  five1 <- fivenum(dfP[dfP$DVar == c,]$DifPrice) # fivenums
  sd1 <- sd(dfP[dfP$DVar == c,]$DifPrice) # sd
  zros = length(which(dfP[dfP$DVar == c,]$DifPrice == 0))/
    (length(dfP[dfP$DVar == c,]$DifPrice))*100 # Number of exact zeroes
  
  sink("salidaV41-2020.txt", append = T)
  print(paste0("--------------- Diff Variety = ", c, "------------"))
  print(numb, include.rownames=F)
  print(five1, include.rownames=F)
  print(sd1, include.rownames=F)
  print(zros, include.rownames=F)
  sink()
  gc()
}


rm(numb, five1, sd1, zros)

gc()


#### ----------------------------- Tables (Annex) -----------------------------



## --------- Table 10 (price levels) ---------

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
write.csv(info, "/clusteruy/home/leandroz/Bases/Border/InfoOriginDatabase-2020.csv", row.names = F)

# Delete and erase garbage
rm (fiveN,nobs,estd,info,sstart,sup,nsup)
gc()


## --------- Table 11 (price difference) ---------

## Number of observations by products, fivenums & sd
# Five numbers
fiveN <- setDT(dfP)[, fivenum(DifPrice), (Product)] # five numbers
fiveN$vars <- rep(c("Minimum", "notU", "Median", "notU2", "Maximum"), len = nrow(fiveN))
fiveN <- dcast(data=fiveN, formula = Product ~ vars, value.var = "V1")
fiveN$notU <- fiveN$notU2 <- NULL
# Number of observations by product
nobs <-  setDT(dfP)[, .N, by = Product] # number of observations
# Standard deviation
estd <- setDT(dfP)[, sd(DifPrice), (Product)] # standard deviations
setnames(estd, old = "V1", new = "Standard-Deviation")
# Number of exact zeroes
h1 = function (x) (length(which(x == 0))/(length(x))*100) ## Total equal zero
z1 <- setDT(dfP)[, h1(DifPrice), (Product)]
setnames(z1, old = "V1", new = "Exact-zeroes") 
# merge all information into one database (fiveN, nobs, estd, z1)
info <- merge(merge(merge(fiveN, nobs, by = "Product"), estd, by = "Product"), z1, by ="Product")
info <- info[,c("Product","Minimum", "Median","Maximum","Standard-Deviation", "N", "Exact-zeroes")]
write.csv(info, "/clusteruy/home/leandroz/Bases/Border/InfoPriceDiff.csv", row.names = F)

# Erase information and collect garbage
rm (nobs,fiveN,estd,z1,info)
gc()


## --------- Table 12 (supermarkets) ---------

# supers <- read.csv("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/Establecimientos.csv", header = T, sep = ',')
# supers <- read.csv("C:/Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/Establecimientos.csv") #windows
# head(supers)
# 
# chains <- unique(supers$chain)
# by(supers, supers$chain, nrow) # # stores
# by(supers[supers$depto =="Montevideo",], supers[supers$depto =="Montevideo",]$chain, nrow) # stores in Montevideo
# by(supers$city, supers$chain, unique) # chains by city
# by(supers$depto, supers$chain, unique) # chains by departamento (state)
# by(supers$Cashiers, supers$chain, mean) # average size (cashiers per store)
# 
# 
# stores <- unique(supers[,c("chain","chain.number")])
# dbftemp <- merge(dbf,stores, by = "chain.number")
# by(dbftemp, dbftemp$chain, nrow) # average size (cashiers per store)



#### ----------------------------- FIGURES  -----------------------------


## --------- Figure 1 (motivation) ---------

dfP2 <- dfP[dfP$Distance < log(2)]
saveRDS(dfP2, "/clusteruy/home/leandroz/Bases/Border/Less01km-2020.rds")


x= 1
h1 = hist(dfP2[dfP2$DVariety == 0,]$DifPrice, breaks = 
            seq(0,200, by = x))
h1$density = h1$counts/sum(h1$counts)*100
h2 = hist(dfP2[dfP2$DVariety == 1,]$DifPrice, breaks = 
            seq(0,200, by = x))
h2$density = h2$counts/sum(h2$counts)*100

dev.off()
#♠par(mar=c(0,6,4,2)+0.1)
pdf("/clusteruy/home/leandroz/Figuras/plot3.pdf", width=8, height=5) 
opar <- par(lwd=1.8)
plot(h1,freq=FALSE, main = "Same Varieties (grey) vs. One Variety Difference (white)", ylim = c(0,40), xlim = c(0,40), 
     cex.main=1.4, cex.lab=1.5,cex.axis=1.2, 
     xlab = "Price Differences (in %)", ylab = "Relative Frequency", col = "grey70", border = F, las = 1)
lines(h2,freq=FALSE, ylim = c(0,40), xlim = c(0,40), lwd=24) #col = scales::alpha('red',.5)
dev.off()

rm(dfP2)
gc()


## --------- Figure 5 (observations by distance) ---------

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
gc()


## --------- Figure 6 (price diff within and between cities) ---------

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
gc()


#### ---------------------- End of script -----------------------------
