# dhs-mdd.R
# Dennis Chao
# April 2022
# Minimum dietary diversity for women (MDD-W) is described in Martin-Prevel et al 2017 (doi: 10.3945/cdn.117.001701)
# MDD for children (MDD or MDDC) is described in https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Minimum_Dietary_Diversity_Minimum_Meal_Frequency_and_Minimum_Acceptable_Diet.htm
# note that MDD was modified in 2017 to include breastmilk. https://apps.who.int/iris/rest/bitstreams/1093537/retrieve
# This code was testing on DHS individual recode data from Nigeria 2018, Nepal 2016, Tajikistan 2017.
# It does not work on Rwanda 2019-2020.

# computeMDDW
# computes MDD-W for all women in "dhsdata" (individual recode) using the DHS "MAP" file as a codebook
# returns data frame, one row for each row in dhsdata, indicating whether the individual has consumed each of the 10 food groups yesterday, the total number of food groups consumed, and an indicator if at least 5 groups were consumed
computeMDDW <- function(dhsdata, dhsmap) {
# women's dietary intake
    list.mddwgroups <- c("grains.whiteroots.tubers.plantains","pulses","nuts.seeds","dairy",
                         "meat.poultry.fish","eggs","dark.green.leafy","other.vitamin.a.fruit.vegetables",
                         "other.vegetables","other.fruits") # MDD-W food groups in order (1-10)
    if (sum(grepl("Respondent ate", dhsmap))>0) {
        list.foods <- dhsmap[grep("Respondent ate", dhsmap[,"Item Label"]),1:2]
    } else if (sum(grepl("Drank/ate", dhsmap))>0) {
        list.foods <- dhsmap[grep("Drank/ate", dhsmap[,"Item Label"]),1:2]
    } else if (sum(grepl("Mother had", dhsmap))>0) {
        list.foods <- dhsmap[grep("Mother had", dhsmap[,"Item Label"]),1:2]
    }
    names(list.foods) <- c("variable","question")
    list.foods$variable <- tolower(list.foods$variable)
    list.foods$mddwname <- NA
    list.foods$mddwname[grepl("milk",tolower(list.foods$question)) & !grepl("- NA$",(list.foods$question))] <- "dairy"
    list.foods$mddwname[grepl("egg",tolower(list.foods$question))] <- "eggs"
    list.foods$mddwname[grepl("(beans)|(peas)",tolower(list.foods$question))] <- "pulses"
    list.foods$mddwname[grepl("dark.+green",tolower(list.foods$question))] <- "dark.green.leafy"
    list.foods$mddwname[grepl("(fish)|(chicken)|(meat)|(organ)",tolower(list.foods$question))] <- "meat.poultry.fish"
    list.foods$mddwname[is.na(list.foods$mddwname) & grepl("insect",tolower(list.foods$question))] <- ""
    list.foods$mddwname[is.na(list.foods$mddwname) & grepl("(dark yellow)|(pumpkin)|(ripe papaya)|(mango)|(persimmon)|(apricot)",list.foods$question)] <- "other.vitamin.a.fruit.vegetables"
    list.foods$mddwname[is.na(list.foods$mddwname) & grepl("(other vegetables)|(green papaya)",tolower(list.foods$question))] <- "other.vegetables"
    list.foods$mddwname[is.na(list.foods$mddwname) & grepl("(banana)|(other fruits)",tolower(list.foods$question))] <- "other.fruits" # are bananas rich in vitamin A?
    list.foods$mddwname[is.na(list.foods$mddwname) & grepl("nuts",tolower(list.foods$question))] <- "nuts.seeds"
    list.foods$mddwname[is.na(list.foods$mddwname) & grepl("(grain)|(bread)|(white root)|(potato)|(tuber)",tolower(list.foods$question))] <- "grains.whiteroots.tubers.plantains"
    list.foods$mddwname[is.na(list.foods$mddwname) & grepl("( oil)|(fried snacks)|(sweets)|(condiment)|(beverages)|(coffee)",tolower(list.foods$question))] <- "" # not an MDD-W group
    list.foods$mddwname[is.na(list.foods$mddwname) & grepl("vegetables.+that ar",list.foods$question)] <- "other.vitamin.a.fruit.vegetables" # hack for Nigeria
    list.foods$mddwgroup <- match(list.foods$mddwname,list.mddwgroups)
    list.foods$mddwgroup[list.foods$mddwname==""] <- 0
    if (sum(is.na(list.foods$mddwname))>0) { # print out food questions not matched
        print("not matched:")
        print(list.foods$question[is.na(list.foods$mddwname)])
        list.foods$mddwgroup[is.na(list.foods$mddwname)] <- 0
    }
    # tally MDD-W foods
    tempdat <- as.data.frame(dhsdata[,list.foods$variable])
    colnames(tempdat) <- list.foods$mddwgroup # make meaningful colnames
    mddwdat <- data.frame() # each column is an MDD-W food group
    for (i in 1:10) {
        if (i==1) {
            if (sum(colnames(tempdat)==i)>1) {
                mddwdat <- data.frame(rowSums(tempdat[,colnames(tempdat)==i]))
            } else {
                mddwdat <- data.frame((tempdat[,colnames(tempdat)==i]))
            }
        } else {
            if (sum(colnames(tempdat)==i)==1) {
                mddwdat <- cbind(mddwdat,(tempdat[,colnames(tempdat)==i]))
            } else {
                mddwdat <- cbind(mddwdat,rowSums(tempdat[,colnames(tempdat)==i]))
            }
        }
    }
    mddwdat[mddwdat>=1] <- 1
    colnames(mddwdat) <- paste("MDDW.group",sprintf("%02d",1:ncol(mddwdat)),sep=".")
    # compute dietary diversity
    mddwdat$MDDWSUM <- rowSums(mddwdat) # how many MDD-W groups consumed?
    mddwdat$MDDW.5 <- (mddwdat$MDDWSUM>=5) # at least 5 consumed (Martin-Prevel 2017)
    mddwdat
}


# computeMDDC
# computes MDD-C for all children in "dhsdata" (child recode), though it should only apply to children ages 6-23m
# assumes food consumption variable names are fixed
# returns data frame, one row for each row in dhsdata, indicating whether the individual has consumed each of the 8 food groups yesterday, the total number of food groups consumed, and an indicator if at least 5 groups were consumed
computeMDDC <- function(dhsdata) {
    list.mddcgroups <- c("breast.milk","grains.roots.tubers","legumes.nuts","dairy",
                         "meat.poultry.fish","eggs","vitamin.a.fruit.vegetables","other.fruits.vegetables")
    list.foods.child <- data.frame(variable = c("v409","v410","v411","v411a","v412a","v412c","v413","v414e","v414f",
                                              "v414g","v414h","v414i","v414j","v414k","v414l","v414m","v414n","v414o",
                                              "v414p","v414s","v414v"),
                                   shortname = c("water","juice","milk","formula",
                                    "fortified.baby.food","soup.broth","other.liquid","grains",
                                    "potatoes.cassava.tubers","eggs","meat","orange.vegetables.or.roots",
                                    "dark.leafy.greens","darkyellow.or.orange.fruits","other.fruits","organ.meat",
                                    "fish.or.seafood","beans.or.peas","milk.products","other.solid","yogurt"),
                                   mddcgroup = c(0,0,4,4,
                                                 2,0,0,2,
                                                 2,6,5,7,
                                                 8,7,8,5,
                                                 5,3,4,0,4))
    list.foods.child$mddcname[list.foods.child$mddcgroup>0] <- list.mddcgroups[list.foods.child$mddcgroup]
    
    # tally MDD-C foods
    tempdat <- as.data.frame(dhsdata[,list.foods.child$variable])
    colnames(tempdat) <- list.foods.child$mddcgroup # make meaningful colnames
    mddcdat <- data.frame(breastfeed=dhsdata$v404) # each column is an MDD-C food group
    for (i in 2:8) {
        if (sum(colnames(tempdat)==i)==1) {
            mddcdat <- cbind(mddcdat,(tempdat[,colnames(tempdat)==i]))
        } else {
            mddcdat <- cbind(mddcdat,rowSums(tempdat[,colnames(tempdat)==i]))
        }
    }
    rm(tempdat)
    mddcdat[mddcdat>=1] <- 1
    colnames(mddcdat) <- paste("MDDC.group",sprintf("%02d",1:ncol(mddcdat)),sep=".")
    # compute dietary diversity
    mddcdat$MDDCSUM <- rowSums(mddcdat) # how many MDD-C groups consumed?
    mddcdat$MDDC.5 <- (mddcdat$MDDCSUM>=5) # at least 5 consumed
    mddcdat
}
