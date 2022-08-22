library(readxl)
library(dplyr)
library(data.table)

db <- readRDS("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2021-instrument/lvs.rds")
colnames(db)

db <- db[,c(5,6,11:14,25,27)]

db$time <- (as.numeric(db$year) -2007) * 12 + as.numeric(db$month) - 3

### Only years 2014 and 2019 are used
years <- c(2014,2019)
db <- db[db$year %in% years, ]

### Add product category

products <- read_excel("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/products.xls")
head(products)
colnames(products)

categ <- distinct(products[, c(1,6,9)], id, Market, Used)
rm(products)
categ <- categ[categ$Used ==1,]
categ$Used <- NULL
colnames(categ) <- c("product","category")

db <- merge(db,categ, by= "product")
rm(categ)


db$variety <- 0
db$variety <- with(db, ave(product, category, time, super, FUN= length)) - 1

db <- db[db$dist_bound1 <=2,]

# change the name of 
db$zona <- as.factor(as.character(db$zona))
levels(db$zona)[levels(db$zona) == "C01"] <- "treated"
levels(db$zona)[levels(db$zona) == "C04"] <- "control"
table(db$zona)
levels(db$zona)


### Stores in all the period

stores14 <- unique(db[db$year ==2014,]$super)
stores19 <- unique(db[db$year ==2019,]$super)
stores1419 <- stores14[stores14 %in% stores19]
db$always_sts <- ifelse(db$super %in% stores1419,1,0)

colnames(db)
colnames(db)[3] <- "chain"

#### Add store information
stores <- read.csv("../../Bases/2021/2019.Establecimientos.RevisadoDC.csv")
strs2 <- read.csv("../../Bases/Establecimientos.csv")
colnames(strs2)
strs2 <- strs2[,c(6,17)]
strs2 <- distinct(strs2,chain,.keep_all = T)
colnames(stores)
stores <- stores[,c(1,6,9,10)]
stores <- merge(stores,strs2,by="chain",all.x = T)
stores$chain.number <- ifelse(stores$chain == "Devoto Express",20,
                              stores$chain.number) 
rm(strs2)
colnames(stores)[2] <- "super"
stores$chain <- NULL

db <- merge(db,stores, by="super", all.x = T)
farmacies <- c("Farmashop","San Roque","Pigalle")
'%!in%' <- function(x,y)!('%in%'(x,y))
db <- db[db$chain %!in% farmacies,]
rm(farmacies,stores)

write.csv(db,"../../Bases/2021/dblvs.csv",row.names = F)


# Plots and regressions ---------------------------------------------------


db <- fread("../../Bases/2021/dblvs.csv")
db$month <- formatC(db$month, width=2, flag="0") # fix 3 numbers
db$yearMonth <- paste0(db$year,"-",db$month)
timtext <- unique(db[,c("time","yearMonth")])
timtext <- timtext[order(timtext$time),]
timmonth <- unique(db[,c("time","month")])
colnames(timmonth)[1] <- "Time"


## Quick plotting information
num.prod <- dcast(as.data.table(db), time ~ zona, fun.aggregate = mean, 
               value.var = "variety")
#num.prod2 <- as.data.table(db)[, .(length(unique(product))),by= time]

plot(num.prod$time,num.prod$treated,
     ylim = c(1.0,1.8),
     type = "l",
     xaxt = "n",
     xlab = "Time",
#     ylab = "Av. # of Varieties p/Category",
     ylab = "",
     pch=19,
     lwd=1.5,
     col="#023FA5",bty='l') # col = "grey25"
lines(num.prod$time,num.prod$control, 
       pch=19,
      lwd=1.5,
       bg="#B4464B",
       col="#B4464B")
axis(1, at=82:153, labels=timtext$yearMonth)

legend("topright",
       legend = c("Promoted Area","Non-promoted Area"),bty = "n", 
       col = c("#023FA5","#B4464B"), #pch = c(19,19), 
       y.intersp = .5)

num.prod <- as.data.table(db)[, .(length(unique(product))),by= time]

plot(num.prod$time,num.prod$V1,
     ylim = c(110,130),
     xlab = "Time",
     ylab = "Number of Varieties",
     pch=19, 
     col="#023FA5",bty='l') # col = "grey25"

### Restrict to stores and products in 2019

stores14 <- unique(db[db$year ==2014,]$super)
stores19 <- unique(db[db$year ==2019,]$super)
stores1419 <- stores14[stores14 %in% stores19]

db <- db[db$super %in% stores1419,]
product19 <- unique(db[db$year == 2014 & db$month ==1,]$product)

db$zona <- factor(db$zona, levels = c("control", "trated"))

#### Regressions


# Varieties
reg <- lfe::felm(variety ~ as.factor(year) * zona| as.factor(chain.number) + as.factor(product)
                  | 0 | super + product,db)
summary(reg)

sink("regressions-2021-lvs.txt", append = T)
print("---------- ------------- -----------")
print("---------- Varieties -----------")
print(summary(reg), include.rownames=F)
print(reg$N, include.rownames=F)
sink()

# Prices

reg2 <- lfe::felm(variety ~ as.factor(year) * zona
             | as.factor(super) + as.factor(product) + as.factor(time)|
               0 | super + product, db)
summary(reg2)
# 
sink("regressions-2021-lvs.txt", append = T)
print("---------- ------------- -----------")
print("---------- Price: first stage -----------")
print(summary(reg2), include.rownames=F)
print(reg2$N, include.rownames=F)
sink()
# 
db$fittv <- reg2$fitted.values

reg3 <- summary(lfe::felm(log(price) ~ fittv
                     | as.factor(super) + as.factor(product) | 
                       0 | super + product, db))
reg3

reg3 <- summary(lfe::felm(log(price) ~ fittv * as.factor(year) * zona
                          | as.factor(super) + as.factor(product) | 
                            0 | super + product, db))
reg3

sink("regressions-2021-lvs.txt", append = T)
print("---------- ------------- -----------")
print("---------- Prices: second stage -----------")
print(reg3, include.rownames=F)
print(reg3$N, include.rownames=F)
sink()
