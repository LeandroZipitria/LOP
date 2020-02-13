# ---------------------------------------------------------------------------------
# The script merge product/prices/stores databases and add variety information
# ---------------------------------------------------------------------------------


#### ----------- Common stuff --------------------


### PART 1: LOAD PRICE DATABASE

# Libraries needed
library(data.table) 
library(dplyr)
library(readxl)


## load price database
dbase <- fread("C://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-original.csv", data.table = F)
dbase <- fread("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-original.csv", data.table = F)
head(dbase)

# Database information --
# Super: store id number
# Product: product id number
# Year: year
# Month: month
# Time: year/month variable (1 being 2007/04, 90 being 2014/09)
# PMode: monthly mode of daily price

table(dbase$Product)
table(dbase$Year)

dbase$moda <- log(dbase$PMode) *100 # Add logs of prices and * 100
dbase$PMode <- NULL


#### Add information of Category ---------------------------

# Product information is on a separate file
products <- read_excel("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/products.xls")
products <- read_excel("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/products.xls")
head(products)

categ <- distinct(products[, c(1,6,9,13)], id, Market, Used,`IPC-class`)

# Database information (products) --
# id: product id number identifier
# Category: the product category of the good
# Used: 1 if keep, 0 is skiped
# IPC-class: broad category to construct instrument

colnames(categ) <- c("Product","Category","Used","IPC.class")
dbase <- merge(dbase, categ, by= "Product")  
head(dbase)
# Restrict database
dbase <- dbase[dbase$Used ==1,]
dbase$Used <- NULL


#### Count number of varieties ---------------------

# For each product, count the number of prices in the same category, less 1
dbase$Variety <- 0
dbase$Variety <- with(dbase, ave(Product, Category, Time, Super, FUN= length)) - 1


#### Add common supermarket information -------------

## Load database
supers <- read.csv("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/Establecimientos.csv", header = T, sep = ',')
supers <- read.csv("C:/Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/Establecimientos.csv") #windows
head(supers)

supers <- supers[, c("Super", "chain.number", "city.number", "X_UTM", "Y_UTM","ccz")] ## the information needed to merge

# Database information (products) --
# Super: store id number identifier
# chain.number: an id of the chain to which the store belongs
# city.number: the id of the city where the store is located
# X_UTM: the location in the easting UTM
# Y_UTM: the location in the northing UTM
# ccz: political location in Montevideo

supers <- supers[order(as.numeric(supers$Super)) ,] # order by supermarket


######## Supermarkets

## Check if all supermarkets are in price frame

supers.supers <- unique(supers$Super)
supers.dbase <- unique(dbase$Super)

'%!in%' <- function(x,y)!('%in%'(x,y)) ## Define function for those not in other vector

not.in.supers <- setdiff(supers.dbase, supers.supers) # supermarkets in price, but not in supers
not.in.prices <- setdiff(supers.supers, supers.dbase) # we do not have information for three supermarkets
rm(supers.dbase, supers.supers)

# Supermarket 386 is repeated, delete it
supers <- supers[supers$Super != 386,]

## Merge databases
# Exclude those supermarkets not in both bases. If want to include only supermarkets
# in price base, include all.x = TRUE

dbase <- merge(dbase, supers, by = "Super") # Merge cola and supermarket bases
rm(supers, not.in.prices, not.in.supers)

## Export databases
write.csv(dbase, "C:/Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-levels-final.csv", row.names = FALSE)
write.csv(dbase, "~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-levels-final.csv", row.names = FALSE)


### Start next script ----------------

source("/path/to/file/2-difference-database-2020.R")

#### End of script ---------------------------------------