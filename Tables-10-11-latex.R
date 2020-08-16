# -- Price database ----

InfoOriginDatabase <- read.csv("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Salidas/2020-v41/InfoOriginDatabase-2020.csv")
InfoOriginDatabase <- read.csv("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Salidas/2020-v41/InfoOriginDatabase-2020.csv")


InfoOriginDatabase$Standard.Deviation <- round(InfoOriginDatabase$Standard.Deviation, 2)
InfoOriginDatabase$Share.stores <- round(InfoOriginDatabase$Share.stores, 0)
InfoOriginDatabase$Minimum <- round(InfoOriginDatabase$Minimum, 2)
InfoOriginDatabase$Median <- round(InfoOriginDatabase$Median, 2)
InfoOriginDatabase$Maximum <- round(InfoOriginDatabase$Maximum, 2)
a <- sum(InfoOriginDatabase$N)
InfoOriginDatabase$N <- format(InfoOriginDatabase$N,big.mark=",")


library(readxl)
products <- read_excel("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/products.xls")
products <- read_excel("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2020.Finales/products.xls")

colnames(products)[1] <- c("Product")
colnames(products)
products <- products[,c(1,6,7)]
library(dplyr)
products <- distinct(products, Product, .keep_all = TRUE) 


InfoOriginDatabase <- merge(InfoOriginDatabase, products, all.x = T, by = "Product")
colnames(InfoOriginDatabase)
InfoOriginDatabase <- InfoOriginDatabase[,c(9,10,2:7)]

InfoOriginDatabase[129,] <- c("TOTAL", "-", "-","-","-","-", a ,"-")
str(InfoOriginDatabase)
InfoOriginDatabase$N <- format(InfoOriginDatabase$N,big.mark=",")


library(xtable)
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n",
                  "\\hline\n",
                  "\\multicolumn{", dim(InfoOriginDatabase)[2] + 1, "}{l}",
                  "{\\footnotesize Continued on next page}\n",
                  "\\endfoot\n",
                  "\\endlastfoot\n")
add.to.row$command <- command
print(xtable(InfoOriginDatabase, align="lllcccccc"),
      hline.after=c(-1), add.to.row = add.to.row,
             include.rownames=F, tabular.environment = "longtable", floating = FALSE)
# to add at the end:   TOTAL & - & - & - & - & - & 2,096,310 & - \\

# -- Price difference database ----
  
pdiff <- read.csv("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Salidas/2020-v41/InfoPriceDiff.csv")
pdiff <- read.csv("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Salidas/2020-v41/InfoPriceDiff.csv")
head(pdiff)

pdiff$Standard.Deviation <- round(pdiff$Standard.Deviation, 2)
pdiff$Exact.zeroes <- round(pdiff$Exact.zeroes, 0)
pdiff$Median <- round(pdiff$Median, 2)
pdiff$Maximum <- round(pdiff$Maximum, 2)
b <- sum(pdiff$N)
pdiff$N <- format(pdiff$N,big.mark=",")

pdiff <- merge(pdiff, products, all.x = T, by = "Product")
colnames(pdiff)
pdiff <- pdiff[,c(8,9,2:7)]
pdiff[129,] <- c("TOTAL", "-", "-","-","-","-", b ,"-")


library(xtable)
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n",
                  "\\hline\n",
                  "\\multicolumn{", dim(pdiff)[2] + 1, "}{l}",
                  "{\\footnotesize Continued on next page}\n",
                  "\\endfoot\n",
                  "\\endlastfoot\n")
add.to.row$command <- command
print(xtable(pdiff, align="lllcccccc"),
      hline.after=c(-1), add.to.row = add.to.row,
      include.rownames=F, tabular.environment = "longtable", floating = FALSE)
#to add at the end:   TOTAL & - & - & - & - & - & 272,370,229 & - \\