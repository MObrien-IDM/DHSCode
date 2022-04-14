# dhs-mdd-example.R
# Dennis Chao
# April 2022
# code to test dhs-mdd.R
library(haven) # for reading Stata files
source("dhs-mdd.R")

# test MDD-W on Nigeria 2018 DHS
womensdata <- read_dta("~/idm/dhs/data/NGIR7BDT/NGIR7BFL.DTA") # Nigeria's 2018 DHS individual recode
womensmap <- read.fwf("~/idm/dhs/data/NGIR7BDT/NGIR7BFL.MAP", widths=c(23,47,7,4,5,6,5,4,5,4), header=FALSE, strip.white=TRUE, skip=17, fileEncoding="latin1") # the "MAP" file is the data dictionary
colnames(womensmap) <- read.fwf("~/idm/dhs/data/NGIR7BDT/NGIR7BFL.MAP", widths=c(23,47,7,4,5,6,5,4,5,4), header=FALSE, strip.white=TRUE, skip=15,n=1)
womensdata <- cbind(womensdata, computeMDDW(womensdata, womensmap))

table(womensdata$MDDW.5) # how many ate at least 5 food categories yesterday?
hist(womensdata$MDDWSUM) # distribution of number of food categories eaten yesterday

# test MDD-C on Nigeria 2018 DHS
childsdata <- read_dta("~/idm/dhs/data/NGKR7BDT/NGKR7BFL.DTA") # Nigeria's 2018 DHS child recode
childsdata <- cbind(childsdata, computeMDDC(childsdata))

table(childsdata$MDDC.5[hw1>=6 & hw1<=23]) # how many ate at least 5 food categories yesterday?
with(childsdata, hist(MDDCSUM[hw1>=6 & hw1<=23])) # distribution of number of food categories eaten yesterday
