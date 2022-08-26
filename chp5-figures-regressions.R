library(data.table)
library(lfe)

dfP <- readRDS(file = "../../Bases/2021/2021_PriceDiff_lvs.rds")
#dfP$DifPrice <- dfP$DifPrice *100
#dfP$Time <- as.integer(as.character(dfP$Time))
#dfP <- merge(dfP,timmonth)
#saveRDS(dfP,file = "../../Bases/2021/2021_PriceDiff_lvs.rds")


## Quick plotting information
num.prod <- dcast(as.data.table(dfP), Time ~ Treated, fun.aggregate = mean, 
                  value.var = "DifPrice")
num.prod$Time <- as.numeric(as.character(num.prod$Time))
num.prod <- num.prod[order(num.prod$Time),]
#num.prod2 <- as.data.table(db)[, .(length(unique(product))),by= time]

plot(num.prod$Time,num.prod$`1`,
#     ylim = c(1.0,1.8),
     type = "l",
     xaxt = "n",
     xlab = "Time",
     ylab = "Av. Price Dispersion (%)",
     pch=19, 
     col="#023FA5",bty='l') # col = "grey25"
axis(1, at=82:153, labels=timtext$yearMonth)



dfP <- dfP[dfP$Treated == 1,]
gc()

# Years 2014 and 2015
reg1 <- felm(DifPrice ~ DVar * as.factor(year)
             | Product + SuperL + SuperR + as.factor(month)| 0 | 
               SuperL + SuperR + Time, data=dfP[dfP$year %in% c(2014,2015)])
summary(reg1)
reg1$N

# Years 2014 and 2016
reg2 <- felm(DifPrice ~ DVar * as.factor(year)
             | Product + SuperL + SuperR + as.factor(month)| 0 | 
               SuperL + SuperR + Time, data=dfP[dfP$year %in% c(2014,2016)])
summary(reg2)

# Years 2014 and 2017
reg3 <- felm(DifPrice ~ DVar * as.factor(year)
             | Product + SuperL + SuperR + as.factor(month)| 0 | 
               SuperL + SuperR + Time, data=dfP[dfP$year %in% c(2014,2017)])
summary(reg3)

# All Years 
reg1 <- felm(DifPrice ~ DVar * as.factor(year)
             | Product + SuperL + SuperR + as.factor(month)| 0 | 
               SuperL + SuperR + Time, data=dfP)
summary(reg1)
reg1$N


sink("regressions-2021-lvs.txt", append = T)
print("---------- ------------- -----------")
print("---------- Diff Varieties: 2014-15 -----------")
print(summary(reg1), include.rownames=F)
print(reg1$N, include.rownames=F)
print("---------- ------------- -----------")
print("---------- Diff Varieties: 2014-16 -----------")
print(summary(reg2), include.rownames=F)
print(reg2$N, include.rownames=F)
print("---------- ------------- -----------")
print("---------- Diff Varieties: 2014-17 -----------")
print(summary(reg3), include.rownames=F)
print(reg3$N, include.rownames=F)
sink()


reg <- felm(DifPrice ~ DVar * as.factor(year)
             | Product + SuperL + SuperR + as.factor(month)| 0 | 
               SuperL + SuperR + Time, data=dfP2[dfP2$AlwaysStr ==1,])
summary(reg2)
