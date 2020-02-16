unlink(".RData")

# ----------------------------------------------------------------------------
# ------------------------------- Regressions --------------------------------
# ----------------------------------------------------------------------------

# dbf original database
# dfP price difference database

library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy
library("lfe", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy
#library(data.table)

### Load databases

dbf <- read.csv("/clusteruy/home/leandroz/Bases/Border/2020-dbase-levels-final.csv")
#dbf <- read.csv("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-levels-final.csv")
gc()


###### --------- Table 4 (Effect of Varieties on Prices) ---------

# First column
reg1 <- felm(moda ~ Variety
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbf)

sink("regressions-2020.txt", append = T)
print("---------- Price equation (factor*time + store*time) -----------")
print(summary(reg1), include.rownames=F)
print(reg1$N)
sink()
gc()

# Second column
reg1 <- felm(moda ~ Variety
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) + 
               as.factor(Product) * as.factor(Super) | 0 | Super + Time, dbf)

sink("regressions-2020.txt", append = T)
print("---------- Price equation (factor*time + store*time + store*product) -----------")
print(summary(reg1), include.rownames=F)
print(reg1$N)
sink()
gc()


###### -------------- Table 5 (Price Differences) --------------

# All regressions are the same, so we apply a function to two dataframes

regression <- function(x) {
  
  sink("regressions-2020.txt", append = T)
  print(str(x))
  print(dim(x))
  sink()
  
  #### Distance regression
  
  # Base regression, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DifCity + SameChain
                       | Product + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020.txt", append = T)
  print("---------- First column -----------")
  print(reg1, include.rownames=F)
  print(reg1$N)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing store fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance
                       | Product + SuperL + SuperR + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020.txt", append = T)
  print("---------- Second column -----------")
  print(reg1, include.rownames=F)
  print(reg1$N)
  sink()
  rm(reg1)
  gc()
  
  #### Variety regression
  
  # Base regression, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DVar + DifCity + SameChain
                       | Product + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020.txt", append = T)
  print("---------- Third column -----------")
  print(reg1, include.rownames=F)
  print(reg1$N)
  sink()
  rm(reg1)
  gc()
  
  # Absorbing store fixed effects, clustered Store + Time
  reg1 <- summary(felm(DifPrice ~ Distance + DVar
                       | Product + SuperL + SuperR + Time | 0 | SuperL + SuperR + Time, x))
  sink("regressions-2020.txt", append = T)
  print("---------- Base Regression NUESTRA (Super FE, clustered Store + time) -----------")
  print(reg1, include.rownames=F)
  print(reg1$N)
  sink()
  rm(reg1)
  gc()
}


##----- Panel A) Random database -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/2020-PriceDiff-random.rds") #ClusterUy
gc()

sink("regressions-2020.txt", append = T)
print("---------- RANDOM database -----------")
sink()

regression(dfP)


##----- Panel B) Year 2011 -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/df2011-inst-2020.rds") #ClusterUy
gc()

sink("regressions-2020.txt", append = T)
print("---------- YEAR 2011 database -----------")
sink()

regression(dfP)


###### -------------- Table 6 (Price differences by distance) --------------

##----- Panel A) 30 kilometers -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/2020-PriceDiff-final.rds") #ClusterUy
dfP <- setDT(dfP)[dfP$Distance <= log(31),] # 30 kilometers
gc()

sink("regressions-2020.txt", append = T)
print("---------- 30 kilometers -----------")
sink()
gc()

regression(dfP)

##----- Panel B) More than 100 kilometers -------------------

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/2020-PriceDiff-final.rds") #ClusterUy
dfP <- setDT(dfP)[dfP$Distance > log(101),] # more than 100 kilometers
gc()

sink("regressions-2020.txt", append = T)
print("---------- +100 kilometers -----------")
sink()
gc()

regression(dfP)

## Erase difference database
rm(dfP)


###### -------------- Table 7 (instrument) --------------

## First column

# First stage
reg2 <- felm(Variety ~ Intrument
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbf)

sel <- which(!is.na(dbf$Intrument))
dbf$fittv <- NA
dbf$fittv[sel] <- reg2$fitted.values

# Second stage
reg3 <- summary(felm(moda ~ fittv
                     | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
                       0 | Super + Time, dbf))

sink("regressions-2020.txt", append = T)
print("---------- INSTRUMENT -----------")
print("---------- Price equation (factor*time + store*time) -----------")
print("---------- First stage -----------")
print(summary(reg2), include.rownames=F)
print("---------- Second stage -----------")
print(reg3, include.rownames=F)
sink()


## Second column

# First stage
reg2 <- felm(Variety ~ Intrument
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) 
             + as.factor(Product) * as.factor(Super) | 0 | Super + Time, dbf)
summary(reg2)

sel <- which(!is.na(dbf$Intrument))
dbf$fittv <- NA
dbf$fittv[sel] <- reg2$fitted.values

# Second stage
reg3 <- summary(felm(moda ~ fittv
                     | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) 
                     + as.factor(Product) * as.factor(Super) | 0 | Super + Time, dbf))
reg3

sink("regressions-2020.txt", append = T)
print("---------- Price equation (factor*time + store*time + store*product) -----------")
print("---------- First stage -----------")
print(summary(reg2), include.rownames=F)
print("---------- Second stage -----------")
print(reg3, include.rownames=F)
sink()


###### -------------- Table 8 (Robustness: chains) --------------

# chain.number == 13 are those stores that do not belong to any chain
# Create two databases
dbfCh <- dbf[dbf$chain.number != 13,]
dbfNCh <- dbf[dbf$chain.number == 13,]


##----- Panel A) Chain stores -------------------

reg1 <- summary(felm(moda ~ Variety
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbfCh))

reg2 <- felm(Variety ~ Intrument
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbfCh) #, exactDOF = 1899148)

sel <- which(!is.na(dbfCh$Intrument))
dbfCh$fittv <- NA
dbfCh$fittv[sel] <- reg2$fitted.values

reg3 <- summary(felm(moda ~ fittv
                     | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
                       0 | Super + Time, dbfCh))

sink("regressions-2020.txt", append = T)
print("---------- ------------- -----------")
print("---------- ROBUSTNESS: CHAINS -----------")
print("---------- CHAIN Stores -----------")
print("---------- Price equation (factor*time + store*time) -----------")
print(reg1, include.rownames=F)
print("---------- First stage -----------")
print(summary(reg2), include.rownames=F)
print("---------- Second stage -----------")
print(reg3, include.rownames=F)
sink()


##----- Panel B) No chains stores  -------------------


reg1 <- summary(felm(moda ~ Variety
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbfNCh))

reg2 <- felm(Variety ~ Intrument
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbfNCh) #, exactDOF = 1899148)

sel <- which(!is.na(dbfNCh$Intrument))
dbfNCh$fittv <- NA
dbfNCh$fittv[sel] <- reg2$fitted.values

reg3 <- summary(felm(moda ~ fittv
                     | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
                       0 | Super + Time, dbfNCh))


sink("regressions-2020.txt", append = T)
print("---------- ------------- -----------")
print("---------- ROBUSTNESS -----------")
print("---------- NO CHAIN Stores -----------")
print("---------- Price equation (factor*time + store*time) -----------")
print(summary(reg1), include.rownames=F)
print("---------- First stage -----------")
print(summary(reg2), include.rownames=F)
print("---------- Second stage -----------")
print(reg3, include.rownames=F)
sink()


###### -------------- Table 9 (Robustness: size) --------------

sup <- fread("/clusteruy/home/leandroz/Bases/Border/Establecimientos.csv", data.table = F)
colnames(sup)
median(sup$Cashiers)


##----- Panel A) Small stores  -------------------

medup <- sup[sup$Cashiers < median(sup$Cashiers),]$Super

reg1 <- felm(moda ~ Variety
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbf[dbf$Super %in% medup,])
summary(reg1)

reg2 <- felm(Variety ~ Intrument
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbf[dbf$Super %in% medup,]) 

sel <- which(!is.na(dbf[dbf$Super %in% medup,]$Intrument))
dbf$fittv <- NA
dbf[dbf$Super %in% medup,]$fittv[sel] <- reg2$fitted.values

reg3 <- summary(felm(moda ~ fittv
                     | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
                       0 | Super + Time, dbf[dbf$Super %in% medup,]))

sink("regressions-2020.txt", append = T)
print("---------- ------------- -----------")
print("---------- ROBUSTNESS -----------")
print("---------- BELOW MEDIAN SIZE -----------")
print("---------- Price equation (factor*time + store*time) -----------")
print(summary(reg1), include.rownames=F)
print("---------- First stage -----------")
print(summary(reg2), include.rownames=F)
print("---------- Second stage -----------")
print(reg3, include.rownames=F)
sink()


##----- Panel B) Larger stores  -------------------

medup <- sup[sup$Cashiers > median(sup$Cashiers),]$Super

reg1 <- felm(moda ~ Variety
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbf[dbf$Super %in% medup,])
summary(reg1)

reg2 <- felm(Variety ~ Intrument
             | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
               0 | Super + Time, dbf[dbf$Super %in% medup,]) 

sel <- which(!is.na(dbf[dbf$Super %in% medup,]$Intrument))
dbf$fittv <- NA
dbf[dbf$Super %in% medup,]$fittv[sel] <- reg2$fitted.values

reg3 <- summary(felm(moda ~ fittv
                     | as.factor(Time) * as.factor(Super) + as.factor(Time) * as.factor(Category) | 
                       0 | Super + Time, dbf[dbf$Super %in% medup,]))

sink("regressions-2020.txt", append = T)
print("---------- ------------- -----------")
print("---------- ROBUSTNESS -----------")
print("---------- ABOVE MEDIAN SIZE -----------")
print("---------- Price equation (factor*time + store*time) -----------")
print(summary(reg1), include.rownames=F)
print("---------- First stage -----------")
print(summary(reg2), include.rownames=F)
print("---------- Second stage -----------")
print(reg3, include.rownames=F)
sink()


###### ----------------- End of script --------------------------------
