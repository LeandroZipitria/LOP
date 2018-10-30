# cargo la libreria de data.table

#library("data.table", lib.loc="~/R/lib/") # FING FING
library("data.table") # CASA CASA
require(data.table) # for fread
library("dplyr")
library(readxl)

########## Previous ###########

## Load database of products and extract information on variety and competition
products <- read_excel("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/lista_productos_2014_web_MEF_empresas_lz.xls")
table(products$`Empresas distintas`)

Variety <- unique(products[products$`Empresas distintas`== "Variedad",]$id)
Mix <- unique(products[products$`Empresas distintas`== "Mix",]$id)
Comp <- unique(products[products$`Empresas distintas`== "Competencia",]$id)
Merge <- unique(products[products$`Empresas distintas`== "Fusion",]$id)

# Count number of products
Productos = length(Variety) + length(Mix) + length(Comp) + length(Merge)

# Merge all databases
AllProducts <- c(Variety, Comp, Merge, Mix)




########## Variety ############

#system.time(precios <- fread("/home/leandroz/2014precios.csv"))  ## FING FING
system.time(dbVariety <- fread("/run/media/lzipitria/TOSHIBA EXT/BASES DE DATOS/DGC bases/2014/2014precios.csv", data.table = F))
            
# pongo los nombres a las columnas

colnames(dbVariety) <- c("Product", "Year", "Month", "Day", "Price", "Super")
head(dbVariety)

## restrict database
dbVariety <- dbVariety[dbVariety$Product %in% Variety,]

dbVariety$Date <- as.Date( paste( dbVariety$Year, dbVariety$Month , dbVariety$Day , sep = "." )  , format = "%Y.%m.%d" )

# Erase prices outside period of analysis
dbVariety <- dbVariety[dbVariety$Date >= "2007-04-01",]
dbVariety <- dbVariety[dbVariety$Date <"2014-10-01",]

# Monthly time dummy
dbVariety$Time <- (as.numeric(dbVariety$Year) -2007) * 12 + as.numeric(dbVariety$Month) - 3 #time dummy
fivenum(dbVariety$Time)

# Erase duplicates
dbVariety <- distinct(dbVariety, Super, Product, Date, .keep_all = TRUE) # erase duplicate

### Outliers
z <- 3 # number of times below or above the median
#dbprueba <- precios

dbVariety$result <- with(dbVariety, ave(dbVariety$Price, dbVariety$Product, dbVariety$Time, FUN=median)) # median monthly price
dbVariety$high <- z*dbVariety$result # upper bound
dbVariety$low <- 1/z * dbVariety$result # lower bound
dbVariety <- subset(dbVariety, dbVariety$Price > dbVariety$low & dbVariety$Price < dbVariety$high) # subseted dataframe
dbVariety$low <- dbVariety$high <- dbVariety$result <- NULL # erase aditional variables

### Calculate mode
dbVariety <- dbVariety %>% group_by(Year, Month, Super, Product) %>% mutate(moda = as.numeric(names(sort(-table(Price)))[1L])) #Calcula moda
head(dbVariety)

# erase aditional variables
dbVariety$Day <- dbVariety$Date <- dbVariety$Price <- NULL

# erase duplicate mode prices
dbVariety <- distinct(dbVariety, Super, Product, Time, .keep_all = TRUE) # pick one observation by month

## Save database
write.csv(dbVariety, file="/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Variety.csv", row.names=FALSE)     ## salvo base



######## Mix ##############

#system.time(precios <- fread("/home/leandroz/2014precios.csv"))  ## FING FING
system.time(dbMix <- fread("/run/media/lzipitria/TOSHIBA EXT/BASES DE DATOS/DGC bases/2014/2014precios.csv", data.table = F))

# pongo los nombres a las columnas

colnames(dbMix) <- c("Product", "Year", "Month", "Day", "Price", "Super")
head(dbMix)

## restrict database
dbMix <- dbMix[dbMix$Product %in% Mix, ]

dbMix$Date <- as.Date( paste( dbMix$Year, dbMix$Month , dbMix$Day , sep = "." )  , format = "%Y.%m.%d" )


# Erase prices outside period of analysis
dbMix <- dbMix[dbMix$Date >= "2007-04-01",]
dbMix <- dbMix[dbMix$Date <"2014-10-01",]

# Monthly time dummy
dbMix$Time <- (as.numeric(dbMix$Year) -2007) * 12 + as.numeric(dbMix$Month) - 3 #time dummy
fivenum(dbMix$Time)

# Erase duplicates
dbMix <- distinct(dbMix, Super, Product, Date, .keep_all = TRUE) # erase duplicate

### Outliers
z <- 3 # number of times below or above the median
#dbprueba <- precios

dbMix$result <- with(dbMix, ave(dbMix$Price, dbMix$Product, dbMix$Time, FUN=median)) # median monthly price
dbMix$high <- z*dbMix$result # upper bound
dbMix$low <- 1/z * dbMix$result # lower bound
dbMix <- subset(dbMix, dbMix$Price > dbMix$low & dbMix$Price < dbMix$high) # subseted dataframe
dbMix$low <- dbMix$high <- dbMix$result <- NULL # erase aditional variables

### Calculate mode
dbMix <- dbMix %>% group_by(Year, Month, Super, Product) %>% mutate(moda = as.numeric(names(sort(-table(Price)))[1L])) #Calcula moda
head(dbMix)

# erase aditional variables
dbMix$Day <- dbMix$Date <- dbMix$Price <- NULL

# erase duplicate mode prices
dbMix <- distinct(dbMix, Super, Product, Time, .keep_all = TRUE) # pick one observation by month

## Save database
write.csv(dbMix, file="/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Mix.csv", row.names=FALSE)     ## salvo base



######## Comp ##############

#system.time(precios <- fread("/home/leandroz/2014precios.csv"))  ## FING FING
system.time(dbComp <- fread("/run/media/lzipitria/TOSHIBA EXT/BASES DE DATOS/DGC bases/2014/2014precios.csv", data.table = F))

# pongo los nombres a las columnas

colnames(dbComp) <- c("Product", "Year", "Month", "Day", "Price", "Super")
head(dbComp)

## restrict database
dbComp <- dbComp[dbComp$Product %in% Comp,]

dbComp$Date <- as.Date( paste( dbComp$Year, dbComp$Month , dbComp$Day , sep = "." )  , format = "%Y.%m.%d" )


# Erase prices outside period of analysis
dbComp <- dbComp[dbComp$Date >= "2007-04-01",]
dbComp <- dbComp[dbComp$Date <"2014-10-01",]

# Monthly time dummy
dbComp$Time <- (as.numeric(dbComp$Year) -2007) * 12 + as.numeric(dbComp$Month) - 3 #time dummy
fivenum(dbComp$Time)

# Erase duplicates
dbComp <- distinct(dbComp, Super, Product, Date, .keep_all = TRUE) # erase duplicate

### Outliers
z <- 3 # number of times below or above the median
#dbprueba <- precios

dbComp$result <- with(dbComp, ave(dbComp$Price, dbComp$Product, dbComp$Time, FUN=median)) # median monthly price
dbComp$high <- z*dbComp$result # upper bound
dbComp$low <- 1/z * dbComp$result # lower bound
dbComp <- subset(dbComp, dbComp$Price > dbComp$low & dbComp$Price < dbComp$high) # subseted dataframe
dbComp$low <- dbComp$high <- dbComp$result <- NULL # erase aditional variables

### Calculate mode
dbComp <- dbComp %>% group_by(Year, Month, Super, Product) %>% mutate(moda = as.numeric(names(sort(-table(Price)))[1L])) #Calcula moda
head(dbComp)

# erase aditional variables
dbComp$Day <- dbComp$Date <- dbComp$Price <- NULL

# erase duplicate mode prices
dbComp <- distinct(dbComp, Super, Product, Time, .keep_all = TRUE) # pick one observation by month


## Save database
write.csv(dbComp, file="/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Comp.csv", row.names=FALSE)     ## salvo base


######## Merge ##############

#system.time(precios <- fread("/home/leandroz/2014precios.csv"))  ## FING FING
system.time(dbMerge <- fread("/run/media/lzipitria/TOSHIBA EXT/BASES DE DATOS/DGC bases/2014/2014precios.csv", data.table = F))

# pongo los nombres a las columnas

colnames(dbMerge) <- c("Product", "Year", "Month", "Day", "Price", "Super")
head(dbMerge)

## restrict database
dbMerge <- dbMerge[dbMerge$Product %in% Merge,]


dbMerge$Date <- as.Date( paste( dbMerge$Year, dbMerge$Month , dbMerge$Day , sep = "." )  , format = "%Y.%m.%d" )


# Erase prices outside period of analysis
dbMerge <- dbMerge[dbMerge$Date >= "2007-04-01",]
dbMerge <- dbMerge[dbMerge$Date <"2014-10-01",]

# Monthly time dummy
dbMerge$Time <- (as.numeric(dbMerge$Year) -2007) * 12 + as.numeric(dbMerge$Month) - 3 #time dummy
fivenum(dbMerge$Time)

# Erase duplicates
dbMerge <- distinct(dbMerge, Super, Product, Date, .keep_all = TRUE) # erase duplicate

### Outliers
z <- 3 # number of times below or above the median
#dbprueba <- precios

dbMerge$result <- with(dbMerge, ave(dbMerge$Price, dbMerge$Product, dbMerge$Time, FUN=median)) # median monthly price
dbMerge$high <- z*dbMerge$result # upper bound
dbMerge$low <- 1/z * dbMerge$result # lower bound
dbMerge <- subset(dbMerge, dbMerge$Price > dbMerge$low & dbMerge$Price < dbMerge$high) # subseted dataframe
dbMerge$low <- dbMerge$high <- dbMerge$result <- NULL # erase aditional variables

### Calculate mode
dbMerge <- dbMerge %>% group_by(Year, Month, Super, Product) %>% mutate(moda = as.numeric(names(sort(-table(Price)))[1L])) #Calcula moda
head(dbMerge)

# erase aditional variables
dbMerge$Day <- dbMerge$Date <- dbMerge$Price <- NULL

# erase duplicate mode prices
dbMerge <- distinct(dbMerge, Super, Product, Time, .keep_all = TRUE) # pick one observation by month

## Save database
write.csv(dbMerge, file="/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Merge.csv", row.names=FALSE)     ## salvo base



###### Merge four databases #####

system.time(dbComp <- fread("/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Comp.csv", data.table = F))
system.time(dbVariety <- fread("/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Variety.csv", data.table = F))
system.time(dbMix <- fread("/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Mix.csv", data.table = F))
system.time(dbMerge <- fread("/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Merge.csv", data.table = F))


db <- rbind(dbComp, dbVariety, dbMix, dbMerge)
db$Variety <- ifelse(db$Product %in% Variety, 1, 0)
db$Comp <- ifelse(db$Product %in% Comp, 1, 0)
db$Mix <- ifelse(db$Product %in% Mix, 1, 0)
db$Merge <- ifelse(db$Product %in% Merge, 1, 0)


write.csv(dbase, file="/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/1_Base_Completa.csv", row.names=FALSE)     ## salvo base

