unlink(".RData") # just for the servers

# ---------------------------------------------------------------------------------
# The script create the difference database 
# ---------------------------------------------------------------------------------


#### ----------- Common stuff --------------------

library("dplyr") ## Library on cluster
library("data.table") ## Library on cluster


##### PART 1 - ADD DIFFERENCES FOR VARIABLES -----------------------------------

# load database
dbase <- fread("../../Bases/2021/dblvs.csv", data.table = F)
#dbf <- readRDS("/clusteruy/home/leandroz/Bases/Convergence/2018BaseResiduals.rds") #ClusterUy
head(dbase)

# Erase unused variables
dbase$year <- dbase$month <- dbase$category <- dbase$dist_bound1 <- NULL
colnames(dbase)

# Log of mode price
dbase$price <- log(dbase$price)

### Previous manipulations 
## Fix the number of product, category, city, department
dbase$super <- formatC(dbase$super, width=3, flag="0") # fix 3 numbers
dbase$chain.number <- formatC(dbase$chain.number, width=2, flag="0") # fix 2 numbers

dbase <- dbase[,c("super","product","time","variety","price","chain.number","always_sts","X_UTM","Y_UTM","zona")]
head(dbase)

dbase$treated <- ifelse(dbase$zona == "treated",1,0)
dbase$zona <- NULL

### Create empty database for price differences
df <- matrix(0,0,10)
colnames(df) <- c("Time", "Product", "DifPrice", "DVar", 
                 "DX_UTM", "DY_UTM", "Chain", "Super", "AlwaysStr","Treated")

## Now create a table with price differences and then stack them and add to 
## the file

for(i in unique(dbase$product)) {
  for(j in unique(dbase$time)) {
      sub <- subset(dbase, dbase$product == i & 
                      dbase$time == j)
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
      
      treat = t(outer(sub[,10], sub[,10], `+`)) # city pair
      diag(treat) = NA
      treat[upper.tri(treat)] <- NA
      

      h1= cbind(c(p))
      h2= cbind(c(var))
      h3= cbind(c(xutm))
      h4= cbind(c(yutm))
      h5= cbind(c(chain))
      h6= cbind(c(sup))
      h7= cbind(c(cit))
      h8= cbind(c(treat))
      h9 = cbind(j, i, h1, h2, h3, h4, h5, h6, h7,h8)
      df <- rbind(df, h9)

      df <- na.omit(df) # Delete prices with value NA
  }
  print(paste0("Ended Product ", i))
}

gc()

## Erase auxiliary information
rm(i,j,h1, h2, h3, h4, h5, h6, h7, h8, h9, p, var, chain, 
   xutm, yutm, sup, cit, sub,treat)
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
dfP$AlwaysStrL <- substr(dfP$AlwaysStr, 1,1)
dfP$AlwaysStrR <- substr(dfP$AlwaysStr, 3,3)
dfP$ChainL <- substr(dfP$Chain, 1,2)
dfP$ChainR <- substr(dfP$Chain, 4,5)
dfP$SuperL <- substr(dfP$Super, 1,3)
dfP$SuperR <- substr(dfP$Super, 5,7)

dfP$AlwaysStr <- dfP$Chain <- dfP$Super <- NULL
gc()

dim(dfP)

### Transform variables using as.factor()
dfP$SameChain <- ifelse(dfP$ChainR == dfP$ChainL, 1,0)
dfP <- setDT(dfP)[, Product:= as.factor(Product)]
### Transfrom from character to numeric
gc()
dfP <- setDT(dfP)[, AlwaysStr:= as.numeric(as.character(AlwaysStrL)) +
                                as.numeric(as.character(AlwaysStrR))]
dfP <- setDT(dfP)[, AlwaysStr:= ifelse(AlwaysStr ==2,1,0)]
dfP$AlwaysStrL <- dfP$AlwaysStrR <- NULL
dfP <- setDT(dfP)[, ChainL:= as.factor(as.character(ChainL))]
gc()
dfP <- setDT(dfP)[, ChainR:= as.factor(as.character(ChainR))]
gc()
dfP <- setDT(dfP)[, SuperL:= as.factor(as.character(SuperL))]
gc()
dfP <- setDT(dfP)[, SuperR:= as.factor(as.character(SuperR))]
gc()
dfP <- setDT(dfP)[, DVar:= as.numeric(as.character(DVar))]

## Save database
saveRDS(dfP, file = "../../Bases/2021/2021_PriceDiff_lvs.rds")


#### End of script ---------------------------------------