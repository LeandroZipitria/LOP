lvs <- readstata13::read.dta13("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2021-instrument/sample_smkt_v2.dta")
head(lvs)
colnames(lvs)
# zonas de LVS
table(lvs$zona)
# C01 es las tratadas !!!
# C04 es el control
table(lvs$year)
lvs <- lvs[lvs$year > 2013,]

lvs$C11_index_street_quality_SEG <- lvs$C11_PER_catehh_bad <- lvs$C11_PER_desocu <- lvs$C11_PER_level_primary <- 
  lvs$C11_PER_owner <- lvs$C11_PER_renter <- lvs$C11_PER_vacant_un <- NULL
lvs <- lvs[,-grep("^LVIS", colnames(lvs))]

lvs$nombarrio <- lvs$barrio <- lvs$id <- NULL

fivenum(lvs$dist_bound1)
fivenum(lvs$dist_bound_0)
lvs$dist_bound_0 <- NULL

saveRDS(lvs,"~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2021-instrument/lvs.rds")
