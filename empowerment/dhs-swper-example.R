# dhs-swper-example.R
# Dennis Chao
# March 2022
# code to test dhs-swper.R

library(haven) # for reading Stata files
source("dhs-swper.R")

womensdata <- read_dta("~/idm/dhs/data/NGIR7BDT/NGIR7BFL.DTA") # Nigeria's 2018 DHS
womensdata <- cbind(womensdata, computeSWPER(womensdata))

table(womensdata$SWPER.EMPOWERMENT.1) # attitude to violence
table(womensdata$SWPER.EMPOWERMENT.2) # social independence
table(womensdata$SWPER.EMPOWERMENT.3) # decision-making
par(mfrow=c(3,1))
hist(womensdata$SWPER.SCORE.1)
hist(womensdata$SWPER.SCORE.2)
hist(womensdata$SWPER.SCORE.3)
