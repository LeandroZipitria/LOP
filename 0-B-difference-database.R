unlink(".RData") # just for the servers

# ---------------------------------------------------------------------------------
# The script create the difference database 
# ---------------------------------------------------------------------------------


#### ----------- Common stuff --------------------

library("dplyr", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## Library on cluster
library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## Library on cluster


##### PART 1 - ADD DIFFERENCES FOR VARIABLES -----------------------------------

# load database
dbase <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-levels-final.csv", data.table = F)
dbase <- fread("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-levels-final.csv", data.table = F)
#dbf <- readRDS("/clusteruy/home/leandroz/Bases/Convergence/2018BaseResiduals.rds") #ClusterUy
head(dbase)

# Erase unused variables
dbase$Year <- dbase$Month <- dbase$Category <- NULL
colnames(dbase)

### Previous manipulations 
## Fix the number of product, category, city, department
dbase$Super <- formatC(dbase$Super, width=3, flag="0") # fix 3 numbers
dbase$chain.number <- formatC(dbase$chain.number, width=2, flag="0") # fix 2 numbers
dbase$city.number <- formatC(dbase$city.number, width=2, flag="0") # fix 2 numbers
dbase$depto.number <- formatC(dbase$depto.number, width=2, flag="0") # fix 2 numbers

dbase <- dbase[,c("Super","Product","Time","Variety","moda","chain.number","city.number","X_UTM","Y_UTM")]
head(dbase)

### Create empty database for price differences
df <- matrix(0,0,9)
colnames(df) <- c("Time", "Product", "DifPrice", "DVar", 
                 "DX_UTM", "DY_UTM", "Chain", "Super", "City")

## Now create a table with price differences and then stack them and add to 
## the file

for(i in unique(dbase$Product)) {
  for(j in unique(dbase$Time)) {
      sub <- subset(dbase, dbase$Product == i & 
                      dbase$Time == j)
      as.matrix(sub)
      
      if (nrow(sub) < 2) {next}
      
      p = t(outer(sub[,5], sub[,5], `-`)) # price difference mode
      diag(p) = NA # difference of prices for the same supermarket do not count (deleted)
      p[upper.tri(p)] <- NA # prices of the uper and lower matrix are repeated (deleted)
      p = abs(p) # get absolute value of price differences
      
      var = t(outer(sub[,4], sub[,4], `-`)) # variety difference
      diag(var) = NA
      var[upper.tri(var)] <- NA
      var = abs(var) # get absolute value of varieties
      
      xutm = t(outer(sub[,8], sub[,8], `-`)) # X_UTM differences
      diag(xutm) = NA
      xutm[upper.tri(xutm)] <- NA
      xutm = xutm * xutm
      
      yutm = t(outer(sub[,9], sub[,9], `-`)) # Y_UTM differences
      diag(yutm) = NA
      yutm[upper.tri(yutm)] <- NA
      yutm = yutm * yutm
      
      chain = t(outer(sub[,6], sub[,6], `paste`)) # chain pair
      diag(chain) = NA
      chain[upper.tri(chain)] <- NA
      
      sup = t(outer(sub[,1], sub[,1], `paste`)) # super pair
      diag(sup) = NA
      sup[upper.tri(sup)] <- NA
      
      cit = t(outer(sub[,7], sub[,7], `paste`)) # city pair
      diag(cit) = NA
      cit[upper.tri(cit)] <- NA
      

      h1= cbind(c(p))
      h2= cbind(c(var))
      h3= cbind(c(xutm))
      h4= cbind(c(yutm))
      h5= cbind(c(chain))
      h6= cbind(c(sup))
      h7= cbind(c(cit))
      h8 = cbind(j, i, h1, h2, h3, h4, h5, h6, h7)
      df <- rbind(df, h8)

      df <- na.omit(df) # Delete prices with value NA
  }
  print(paste0("Ended Product ", i))
}

gc()

## Erase auxiliary information
rm(i,j,h1, h2, h3, h4, h5, h6, h7, h8, p, var, chain, 
   xutm, yutm, sup, cit, sub)
gc()

# Create a data frame
dfP <- as.data.frame(df) # Transform the vector into a data.frame
rm(df)
head(dfP)
dim(dfP)
gc()


##### PART 2 - CALCULATE DISTANCE AND CREATE OTHER DUMMIES ------------------------------

dfP$DifPrice <- as.numeric(as.character(dfP$DifPrice))
dfP$DX_UTM <- as.numeric(as.character(dfP$DX_UTM))
dfP$DY_UTM <- as.numeric(as.character(dfP$DY_UTM))
gc()

dfP$Distance <- log(1+(sqrt(dfP$DX_UTM + dfP$DY_UTM)/1000))
head(dfP)
dfP <- dfP[, -c(5,6)] # Delete X_UTM and Y_UTM
dfP$CityL <- substr(dfP$City, 1,2)
dfP$CityR <- substr(dfP$City, 4,5)
dfP$ChainL <- substr(dfP$Chain, 1,2)
dfP$ChainR <- substr(dfP$Chain, 4,5)
dfP$SuperL <- substr(dfP$Super, 1,3)
dfP$SuperR <- substr(dfP$Super, 5,7)

dfP$City <- dfP$Chain <- dfP$Super <- NULL
gc()

dfP$DifCity <- ifelse(dfP$CityL != dfP$CityR, 1, 0)

dim(dfP)

### Transform variables using as.factor()
dfP$SameChain <- ifelse(dfP$ChainR == dfP$ChainL, 1,0)
dfP <- setDT(dfP)[, Product:= as.factor(Product)]
### Transfrom from character to numeric
gc()
dfP <- setDT(dfP)[, CityL:= as.factor(as.character(CityL))]
gc()
dfP <- setDT(dfP)[, CityR:= as.factor(as.character(CityR))]
gc()
dfP <- setDT(dfP)[, DifCity:= ifelse(as.numeric(as.factor(CityL)) == 
                                       as.numeric(as.factor(CityR)), 0,1)]
dfP <- setDT(dfP)[, DifCity:= as.factor(DifCity)]
gc()
dfP <- setDT(dfP)[, ChainL:= as.factor(as.character(ChainL))]
gc()
dfP <- setDT(dfP)[, ChainR:= as.factor(as.character(ChainR))]
gc()
dfP <- setDT(dfP)[, SuperL:= as.factor(as.character(SuperL))]
gc()
dfP <- setDT(dfP)[, SuperR:= as.factor(as.character(SuperR))]


## Save database
saveRDS(dfP, file = "/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff.rds")


## Call next script
source("/path/to/file/0-C-instrument-2020.R")

#### End of script ---------------------------------------