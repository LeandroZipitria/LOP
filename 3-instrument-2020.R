unlink(".RData") # just for the servers

# ---------------------------------------------------------------------------------
# The script create the instrument 
# ---------------------------------------------------------------------------------

library(data.table)
library(readxl)
library(dplyr)

dbase <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-levels-final.csv", data.table = F)
dbase <- fread("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-levels-final.csv", data.table = F)
head(dbase)

## Create instrument -----------------------------------

n = nrow(dbase)

for(i in 1:n) {
  cate <- dbase$Category[i]
  supe = dbase$Super[i]
  IPCc = dbase$IPC.class[i]
  city = dbase$city.number[i]
  cczM <- dbase$ccz[i]
  time <- dbase$Time[i]
  
  if(city == 30) {
    test <- subset(dbase, Time == time & IPC.class == IPCc & ccz == cczM    # included
                   & Category != cate & Super != supe,                         # excluded
                   select = Variety)                                           # select variety           
  } else {
    test <- subset(dbase, Time == time & IPC.class == IPCc  & city.number == city    # included
                   & Category != cate & Super != supe,                                 # excluded
                   select = Variety)                                                   # select Variety                    
  }
  
  dbase$Instrument[i] <- mean(test$Variety)
}

# save database
write.csv(dbase, "~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/2020-dbase-levels-final.csv", row.names = FALSE)


## Call next script
source("/path/to/file/3-figures-tables-2020.R")

#### End of script ---------------------------------------
