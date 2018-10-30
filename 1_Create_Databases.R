################################################
### SCRIPT TO CREATE PRICE DIFFERENCES #########
################################################


########### Common stuff  ################

##### PART 1: LOAD PRICE DATABASE

library("data.table") 
library("dplyr")
## load price database
dbase <- fread("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Completa.csv", data.table = F)
#dbase <- fread("C:/Users/usuario/Dropbox/2016.Distance and quality/Bases/db_clean_all.csv", data.table = F) #Windows
head(dbase)
table(dbase$Product)

# order the database
dbase <- dbase[order(dbase$Super, dbase$Year, dbase$Month),]

###

## logs of prices and * 100
dbase$moda <- log(dbase$moda) *100


## Add information of Category
products <- read_excel("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/lista_productos_2014_web_MEF_empresas_lz.xls")
categ <- distinct(products[, c(1,5)], id, Producto)
colnames(categ) <- c("Product", "Category")
dbase <- merge(dbase, categ, by= "Product")  



#################################################################
##### PART 2: ADD ADITIONAL INFORMATION TO THE EACH DATABASE ####


#### 1) Variety ####

dbase$varietyNone <- 0
dbase$varietyNone <- with(dbase, ave(Product, Category, Time, Super, FUN= length)) - 1
dbase$varietyNone <- dbase$varietyNone * dbase$Variety

## Check information
table(dbase$Category, dbase$varietyNone)
table(dbase$varietyNone)


#### 2) Competition ####

dbase$competitionAll <- with(dbase, ave(Product, Category, Year, Month, Super, FUN= length)) - 1
dbase$competitionAll <- dbase$competitionAll * dbase$Comp

## Check information
table(dbase$Category, dbase$competitionAll)


########### 3) Some ################

# But Rice!
multiproducer <- c(1,3,10,12,26,27,35,36,68,70,79,81,82,83,88,89,
                   94,95,103,104,106,107,108,109,119,120,125,126,134,135,149,150) # include merger
onlyproducer <- c(2,11,28,34,69,80,84,90,96,102,105,110,118,127,133,151)

## First: calculate the number of competitors for those producers that have only one product
dbase$onlyproducer <- ifelse(dbase$Product %in% onlyproducer, 1, 0)
dbase$competitionSome <- with(dbase, ave(Product, Category, Time, Super, FUN= length)) - 1
dbase$competitionSome <- dbase$competitionSome * dbase$onlyproducer
table(dbase$competitionSome)
table(dbase$competitionSome, dbase$Product)


## Second: calculate the number of varieties for multiproducers
dbase$multiproducer <- ifelse(dbase$Product %in% multiproducer, 1, 0)
dbase$varietySome <- 0
dbase[dbase$multiproducer == 1,]$varietySome <- with(dbase[dbase$multiproducer == 1,], ave(Product, Category, Time, Super, FUN= length)) - 1
dbase$varietySome <- dbase$varietySome * dbase$multiproducer
table(dbase$varietySome)


## Third: calculate the number of competitors for each variety of multiproducer
multip1 <- c(1,10,26,35,68,79,82,88,94,103,106,108,119,125,134,149)
multip2 <- c(3,12,27,36,70,81,83,89,95,104,107,109,120,126,135,150)

dbase$multip1 <- ifelse(dbase$Product %in% multip1, 1, 0)
dbase$multip2 <- ifelse(dbase$Product %in% multip2, 1, 0)

dbase$competitionSome1 <-0
dbase[dbase$multip2 == 0,]$competitionSome1 <- with(dbase[dbase$multip2 == 0,], ave(Product, Category, Time, Super, FUN= length)) - 1
dbase$competitionSome1 <- dbase$competitionSome1 * dbase$multip1
table(dbase$competitionSome1)
table(dbase$competitionSome1, dbase$Product)

dbase$competitionSome2 <-0
dbase[dbase$multip1 == 0,]$competitionSome2 <- with(dbase[dbase$multip1 == 0,], ave(Product, Category, Time, Super, FUN= length)) - 1
dbase$competitionSome2 <- dbase$competitionSome2 * dbase$multip2
table(dbase$competitionSome2)
table(dbase$competitionSome2, dbase$Product)


## Fourth: rice market

rice1 <- c(15,16,20)
rice2 <- c(17,18,19)

# Variety for each rice firm
dbase$rice1 <- ifelse(dbase$Product %in% rice1, 1, 0)
dbase$rice2 <- ifelse(dbase$Product %in% rice2, 1, 0)

dbase$varietyRice1 <- 0
dbase[dbase$rice1 == 1,]$varietyRice1 <- with(dbase[dbase$rice1 == 1,], ave(Product, Category, Time, Super, FUN= length)) - 1
dbase$varietyRice1 <- dbase$varietyRice1 * dbase$rice1
table(dbase$varietyRice1)

dbase$varietyRice2 <- 0
dbase[dbase$rice2 == 1,]$varietyRice2 <- with(dbase[dbase$rice2 == 1,], ave(Product, Category, Time, Super, FUN= length)) - 1
dbase$varietyRice2 <- dbase$varietyRice2 * dbase$rice2
table(dbase$varietyRice2)


## Competition for each brand

dbase$competitionRice1 <- 0
for (i in rice1) {
  dbase[dbase$Product == i | dbase$rice2 == 1,]$competitionRice1 <- with(dbase[dbase$Product == i | dbase$rice2 == 1,], 
                                                                          ave(Product, Time, Super, FUN= length)) - 1
}
dbase$competitionRice1 <- dbase$competitionRice1 * dbase$rice1
table(dbase$competitionRice1)

dbase$competitionRice2 <- 0
for (i in rice2) {
  dbase[dbase$Product == i | dbase$rice1 == 1,]$competitionRice2 <- with(dbase[dbase$Product == i | dbase$rice1 == 1,], 
                                                                         ave(Product, Time, Super, FUN= length)) - 1
}
dbase$competitionRice2 <- dbase$competitionRice2 * dbase$rice2
table(dbase$competitionRice2)


##### 4) Merge ######

### There are mergers on two markets: toothpaste (49) and bread (71)

## Pre and post merger periods

dbase$mergerT1 <- ifelse(dbase$Time < 71,1,0)
dbase$mergerT2 <- ifelse(dbase$mergerT1 == 1,0,1)
dbase$mergerB1 <- ifelse(dbase$Time < 49,1,0)
dbase$mergerB2 <- ifelse(dbase$mergerB1 == 1,0,1)


# Post merger: all products are of the same firm
dbase$varietyMergerTEP <- 0
dbase[dbase$Category == "Pasta de dientes",]$varietyMergerTEP <- 
  with(dbase[dbase$Category == "Pasta de dientes",], ave(Product, Time, Super, FUN= length)) - 1
dbase$varietyMergerTEP <- dbase$varietyMergerTEP * dbase$mergerT2
table(dbase$varietyMergerTEP)

dbase$varietyMergerBEP <- 0
dbase[dbase$Category == "Pan de molde",]$varietyMergerBEP <- 
  with(dbase[dbase$Category == "Pan de molde",], ave(Product, Time, Super, FUN= length)) - 1
dbase$varietyMergerBEP <- dbase$varietyMergerBEP * dbase$mergerB2
table(dbase$varietyMergerBEP)


# Pre merger: some products are from different firm (previously calculated)

dbase$Tooth <- ifelse(dbase$Category == "Pasta de dientes",1,0) # create dummy for each market
dbase$Tooth <- dbase$Tooth * dbase$mergerT2  # restrict dummy to post merger period
dbase$Bread <- ifelse(dbase$Category == "Pan de molde",1,0)
dbase$Bread <- dbase$Bread * dbase$mergerB2 
dbase$PreMerge <- 1 - dbase$Tooth - dbase$Bread # create a 1 for all products, and for merge products just for ex ante
a = sum(dbase$Tooth)
b = sum(dbase$Bread)
c = a + b
table(dbase$PreMerge)

dbase$competitionSome <- dbase$competitionSome * dbase$PreMerge
dbase$varietySome <- dbase$varietySome * dbase$PreMerge
dbase$competitionSome1 <- dbase$competitionSome1 * dbase$PreMerge
dbase$competitionSome2 <- dbase$competitionSome2 * dbase$PreMerge


#### Erase aditional information and sum columns ####

head(dbase)

# sum
dbase$competition <- 0
dbase$competition <- dbase$competitionAll + dbase$competitionRice1 + dbase$competitionRice2 +
  dbase$competitionSome + dbase$competitionSome1 + dbase$competitionSome2
table(dbase$competition)

dbase$variety <- 0
dbase$variety <- dbase$varietyNone + dbase$varietyRice1 + dbase$varietyRice2 + 
  dbase$varietySome + dbase$varietyMergerTEP + dbase$varietyMergerBEP
table(dbase$variety)

# Erase
dbase$varietyNone <- dbase$competitionAll <- dbase$onlyproducer <- dbase$competitionSome <-
  dbase$varietySome <- dbase$multip1 <- dbase$multip2 <- dbase$competitionSome1 <- 
  dbase$competitionSome2 <- dbase$rice1 <- dbase$rice2 <- dbase$varietyRice1 <- 
  dbase$varietyRice2 <- dbase$varietyMergerT <- dbase$mergerT1 <- dbase$mergerT2 <-
  dbase$mergerB1  <- dbase$mergerB2  <- dbase$varietyMergerBEP <- dbase$varietyMergerTEP <-
  dbase$competitionSomeM <- dbase$competitionRice1 <- dbase$competitionRice2 <- 
  dbase$Tooth <- dbase$Bread <- dbase$PreMerge <- dbase$ multiproducer <- NULL



#### Add common supermarket information ####

####### Load information about supermarkets

## Load database
supers <- read.csv("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/Establecimientos.csv", header = T, sep = ',')
#supers <- read.csv("C:/Users/usuario/Dropbox/2016.Distance and quality/Bases/Establecimientos.csv") #windows
head(supers)

supers <- supers[, c("Super", "chain.number", "city.number", "depto.number", "X_UTM", "Y_UTM")] ## the information needed to merge
supers <- supers[order(as.numeric(supers$Super)) ,] # order by supermarket

######## Supermarkets

## Check if all supermarkets are in price frame

supers.supers <- unique(supers$Super)
supers.dbase <- unique(dbase$Super)

'%!in%' <- function(x,y)!('%in%'(x,y)) ## Define function for those not in other vector

not.in.supers <- setdiff(supers.dbase, supers.supers) # supermarkets in price, but not in supers
not.in.prices <- setdiff(supers.supers, supers.dbase)
rm(supers.dbase, supers.supers)

# Supermarket 386 is repeated, delete it
supers <- supers[supers$Super != 386,]

## Merge databases
# Exclude those supermarkets not in both bases. If want to include only supermarkets
# in price base, include all.x = TRUE

dbase <- merge(dbase, supers, by = "Super") # Merge cola and supermarket bases
rm(supers, not.in.prices, not.in.supers)


# Add Montevideo dummy
#dbase$Montevideo <- ifelse(dbase$city == "Montevideo", 1, 0)

## Export databases
write.csv(dbase, "~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/2018_dbase_with_super.csv", row.names = FALSE)



