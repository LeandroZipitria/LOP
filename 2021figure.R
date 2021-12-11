unlink(".RData")

# ----------------------------------------------------------------------------
# ---------------------------- Tables and Figures ----------------------------
# ----------------------------------------------------------------------------

# dbf original database
# dfP price difference database

library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib')) ## ClusterUy
#library(data.table)

### Load databases

dfP <-  readRDS("/clusteruy/home/leandroz/Bases/Border/Old/2018_PriceDiff.rds") #ClusterUy
str(dfP)
head(dfP)


## --------- Figure 1 (motivation) ---------

dfP1 <- dfP[dfP$Distance > log(200),]
dfP2 <- dfP[dfP$Distance < log(5),]
rm(dfP)
#saveRDS(dfP2, "/clusteruy/home/leandroz/Bases/Border/Less01km-2020.rds")

x= 1
h1 = hist(dfP1$DifPrice, breaks = 
            seq(0,200, by = x))
h1$density = h1$counts/sum(h1$counts)*100
h2 = hist(dfP2$DifPrice, breaks = 
            seq(0,200, by = x))
h2$density = h2$counts/sum(h2$counts)*100

dev.off()
pdf("/clusteruy/home/leandroz/Figuras/plot4_5.pdf", width=8, height=5) 
opar <- par(lwd=1.8)
plot(h1,freq=FALSE, main = "Price Difference: Distance greater than 200 kilometers (grey) \
     Less than 5 kilometers (white)", ylim = c(0,40), xlim = c(0,40), 
     cex.main=1.4, cex.lab=1.5,cex.axis=1.2, 
     xlab = "Price Differences (in %)", ylab = "Relative Frequency", col = "grey70", border = F, las = 1)
lines(h2,freq=FALSE, ylim = c(0,40), xlim = c(0,40), lwd=24) #col = scales::alpha('red',.5)
dev.off()
gc()


# --------- Figure 1: 2008 (motivation) ---------

y2008 <- c(12:21)
y2013 <- c(60:72)

dfP1_08 <- dfP1[dfP1$Time %in% y2008,]
dfP2_08 <- dfP2[dfP2$Time %in% y2008,]
dfP1_13 <- dfP1[dfP1$Time %in% y2013,]
dfP2_13 <- dfP2[dfP2$Time %in% y2013,]
#saveRDS(dfP2, "/clusteruy/home/leandroz/Bases/Border/Less01km-2020.rds")

#### Year 2008

x= 1
h1 = hist(dfP1_08$DifPrice, breaks = 
            seq(0,200, by = x))
h1$density = h1$counts/sum(h1$counts)*100
h2 = hist(dfP2_08$DifPrice, breaks = 
            seq(0,200, by = x))
h2$density = h2$counts/sum(h2$counts)*100

dev.off()
pdf("/clusteruy/home/leandroz/Figuras/plot4_2008_5.pdf", width=8, height=5) 
opar <- par(lwd=1.8)
plot(h1,freq=FALSE, main = "Price Difference: Distance greater than 200 kilometers (grey) \
     Less than 5 kilometers (white), Year 2008", ylim = c(0,40), xlim = c(0,40), 
     cex.main=1.4, cex.lab=1.5,cex.axis=1.2, 
     xlab = "Price Differences (in %)", ylab = "Relative Frequency", col = "grey70", border = F, las = 1)
lines(h2,freq=FALSE, ylim = c(0,40), xlim = c(0,40), lwd=24) #col = scales::alpha('red',.5)
dev.off()
gc()


#### Year 2013

x= 1
h1 = hist(dfP1_13$DifPrice, breaks = 
            seq(0,200, by = x))
h1$density = h1$counts/sum(h1$counts)*100
h2 = hist(dfP2_13$DifPrice, breaks = 
            seq(0,200, by = x))
h2$density = h2$counts/sum(h2$counts)*100

dev.off()
pdf("/clusteruy/home/leandroz/Figuras/plot4_2013_5.pdf", width=8, height=5) 
opar <- par(lwd=1.8)
plot(h1,freq=FALSE, main = "Price Difference: Distance greater than 200 kilometers (grey) \
     Less than 5 kilometers (white), Year 2013", ylim = c(0,40), xlim = c(0,40), 
     cex.main=1.4, cex.lab=1.5,cex.axis=1.2, 
     xlab = "Price Differences (in %)", ylab = "Relative Frequency", col = "grey70", border = F, las = 1)
lines(h2,freq=FALSE, ylim = c(0,40), xlim = c(0,40), lwd=24) #col = scales::alpha('red',.5)
dev.off()

rm(dfP1,dfP2)
gc()