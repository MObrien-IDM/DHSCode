# dhs-mdd-example.R
# Dennis Chao
# April 2022
# code to test dhs-mdd.R
library(haven) # for reading Stata files
source("dhs-mdd.R")

womensdata <- read_dta("~/idm/dhs/data/NGIR7BDT/NGIR7BFL.DTA") # Nigeria's 2018 DHS
womensdata <- cbind(womensdata, computeMDDW(womensdata))

table(womensdata$MDDW.5) # how many ate at least 5 food categories yesterday?
hist(womensdata$MDDWSUM) # distribution of number of food categories eaten yesterday

childsdata <- read_dta("~/idm/dhs/data/NGKR7BDT/NGKR7BFL.DTA") # Nigeria's 2018 DHS
childsdata <- cbind(childsdata, computeMDDC(childsdata))

table(childsdata$MDDC.5[hw1>=6 & hw1<=23]) # how many ate at least 5 food categories yesterday?
with(childsdata, hist(MDDCSUM[hw1>=6 & hw1<=23])) # distribution of number of food categories eaten yesterday
