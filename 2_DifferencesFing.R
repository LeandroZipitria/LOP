unlink(".RData") 

library("dplyr", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy


###############################
####### Do price differences


#################################################
##### PART 1 - ADD DIFFERENCES FOR VARIABLES ####

# load database
#dbf <- fread("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/2018BaseResiduals.csv", data.table = F)
dbf <-  get(load("/clusteruy/home/leandroz/Bases/Convergence/2018BaseResiduals.Rdata")) #ClusterUy
head(dbf)



# Erase unused variables
dbf$RSuperCV <- dbf$RSuper <- dbf$RCompVar <- dbf$RSuperCC <- dbf$RSuperTodo <- dbf$RSuperCCCV <- NULL
dbf$Year <- dbf$Month <- dbf$Variety <- dbf$Comp <- dbf$Merge <- dbf$Mix <- NULL
dbf$Category <- NULL
colnames(dbf)

## Fix the number of product, category, city, department
dbf$Super <- formatC(dbf$Super, width=3, flag="0") # fix 3 numbers
#dbf$Product <- formatC(dbf$Product, width=3, flag="0") # fix 3 numbers
dbf$chain.number <- formatC(dbf$chain.number, width=2, flag="0") # fix 2 numbers
dbf$city.number <- formatC(dbf$city.number, width=2, flag="0") # fix 2 numbers
dbf$depto.number <- formatC(dbf$depto.number, width=2, flag="0") # fix 2 numbers
#dbf$Time <- as.factor(dbf$Time)


### Create empty database for price differences
df <- matrix(0,0,11)
colnames(df) <- c("Time", "Product", "DifPrice", "DComp", "DVariety", 
                 "DX_UTM", "DY_UTM", "Chain", "Super", "City", "Dpto")

## Now create a table with price differences and then stack them and add to 
## the file

for(i in unique(dbf$Product)) {
  for(j in unique(dbf$Time)) {
      sub <- subset(dbf, dbf$Product == i & 
                      dbf$Time == j)
      as.matrix(sub)
      
      if (nrow(sub) < 2) {next}
      
      p = t(outer(sub[,4], sub[,4], `-`)) # price difference mode
      diag(p) = NA # difference of prices for the same supermarket do not count (deleted)
      p[upper.tri(p)] <- NA # prices of the uper and lower matrix are repeated (deleted)
      p = abs(p) # get absolute value of price differences
      
      comp = t(outer(sub[,5], sub[,5], `-`)) # competition difference
      diag(comp) = NA
      comp[upper.tri(comp)] <- NA
      comp = abs(comp) # get absolute value of competition
      
      var = t(outer(sub[,6], sub[,6], `-`)) # variety difference
      diag(var) = NA
      var[upper.tri(var)] <- NA
      var = abs(var) # get absolute value of variety
      
      xutm = t(outer(sub[,10], sub[,10], `-`)) # X_UTM differences
      diag(xutm) = NA
      xutm[upper.tri(xutm)] <- NA
      xutm = xutm * xutm
      
      yutm = t(outer(sub[,11], sub[,11], `-`)) # Y_UTM differences
      diag(yutm) = NA
      yutm[upper.tri(yutm)] <- NA
      yutm = yutm * yutm
      
      chain = t(outer(sub[,7], sub[,7], `paste`)) # chain pair
      diag(chain) = NA
      chain[upper.tri(chain)] <- NA
      
      sup = t(outer(sub[,1], sub[,1], `paste`)) # super pair
      diag(sup) = NA
      sup[upper.tri(sup)] <- NA
      
      cit = t(outer(sub[,8], sub[,8], `paste`)) # city pair
      diag(cit) = NA
      cit[upper.tri(cit)] <- NA
      
      dep = t(outer(sub[,9], sub[,9], `paste`)) # department pair
      diag(dep) = NA
      dep[upper.tri(dep)] <- NA
      
      h1= cbind(c(p))
      h2= cbind(c(comp))
      h3= cbind(c(var))
      h4= cbind(c(xutm))
      h5= cbind(c(yutm))
      h6= cbind(c(chain))
      h7= cbind(c(sup))
      h8= cbind(c(cit))
      h9= cbind(c(dep))
      h10 = cbind(j, i, h1, h2, h3, h4, h5, h6, h7, h8, h9)
      df <- rbind(df, h10)
      
      df <- na.omit(df) # Delete prices with value NA
  }
  print(paste0("Ended Product ", i))
}


   ## Done


## Erase auxiliary info
rm(i,j,h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, p, comp, var, chain, 
   xutm, yutm, sup, dep, cit, sub)

# Create a data frame
dfP <- as.data.frame(df) # Transform the vector into a data.frame
rm(df)
dim(dfP)

###############################################################
##### PART 2 - CALCULATE DISTANCE AND CREATE OTHER DUMMIES ####

dfP$DifPrice <- as.numeric(as.character(dfP$DifPrice))
dfP$DX_UTM <- as.numeric(as.character(dfP$DX_UTM))
dfP$DY_UTM <- as.numeric(as.character(dfP$DY_UTM))


dfP$Distance <- log(1+(sqrt(dfP$DX_UTM + dfP$DY_UTM)/1000))
head(dfP)
dfP <- dfP[, -c(6,7)] # Delete X_UTM and Y_UTM
dfP$CityL <- substr(dfP$City, 1,2)
dfP$CityR <- substr(dfP$City, 4,5)
dfP$ChainL <- substr(dfP$Chain, 1,2)
dfP$ChainR <- substr(dfP$Chain, 4,5)
dfP$SuperL <- substr(dfP$Super, 1,3)
dfP$SuperR <- substr(dfP$Super, 5,7)
dfP$DptoL <- substr(dfP$Dpto, 1,2)
dfP$DptoR <- substr(dfP$Dpto, 4,5)

dfP$Dpto <- dfP$City <- dfP$Chain <- dfP$Super <- NULL


dfP$DifCity <- ifelse(dfP$CityL != dfP$CityR, 1, 0)

dim(dfP)

## Save database
save(dfP, file = "/clusteruy/home/leandroz/Bases/Border/2018_PriceDiff2.Rdata")
#write.csv(dfP, "/home/leandroz/Bases/Convergence/2018BaseDiff.csv", row.names = FALSE)

## Done
#########################################################

## Call next script

source("/clusteruy/home/leandroz/3_TablesFigures.R")
