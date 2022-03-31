# dhs-mdd.R
# Dennis Chao
# March 2022
# Minimum dietary diversity for women (MDD-W) is described in Martin-Prevel et al 2017 (doi: 10.3945/cdn.117.001701)
# MDD for children (MDD or MDDC) is described in https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Minimum_Dietary_Diversity_Minimum_Meal_Frequency_and_Minimum_Acceptable_Diet.htm
# note that MDD was modified in 2017 to include breastmilk. https://apps.who.int/iris/rest/bitstreams/1093537/retrieve

# computeMDDW
# computes MDD-W for all women in "dhsdata"
# returns data frame, one row for each DHS entry, indicating whether the individual has consumed each of the 10 food groups yesterday, the total number of food groups consumed, and an indicator if at least 5 groups were consumed
computeMDDW <- function(dhsdata,dhsmap) {
# women's dietary intake
    list.mddwgroups <- c("grains.whiteroots.tubers.plantains","pulses","nuts.seeds","dairy",
                         "meat.poultry.fish","eggs","dark.green.leafy","other.vitamin.a.fruit.vegetables",
                         "other.vegetables","other.fruits")
    list.foods <- data.frame(variable = c("s653a","s653b","s653c","s653d","s653e","s653f","s653g","s653h","s653i",
                                          "s653j","s653k","s653l","s653m","s653n","s653o","s653p","s653q","s653r",
                                          "s653s","s653t","s653u","s653v"),
                             shortname = c("grains","orange.vegetables.or.roots","white.roots.tubers.plantains", "dark.leafy.greens", # a-d
                                            "darkyellow.or.orange.fruits", "other.fruits", "other.vegetables","organ.meat", # e-h
                                            "other.meat.or.poultry","eggs","fish.or.seafood","beans.or.peas", # i-l
                                            "nuts.or.seeds","milk.products","insects","red.palm.oil", #m-p
                                            "other.oils.and.fats","savoury.fried.snacks","sweets","sugar.sweetened.beverages", #q-t
                                            "condiments.and.seasonings","other.beverages.and.foods"), #u-v. Manually set the names of the foods associated with each variable
                             mddwgroup = c(1,8,1,7,
                                            8,10,9,5,
                                            5,6,5,2,
                                            3,4,0,0,
                                            0,0,0,0,
                                            0,0)) # Manually set the MDD-W food group associated with each variable. Note that insects and red palm oil do not count towards MDD-W
    list.foods$mddwname[list.foods$mddwgroup>0] <- list.mddwgroups[list.foods$mddwgroup]

# compute MDD-W
    tempdat <- as.data.frame(dhsdata[,list.foods$variable])
    colnames(tempdat) <- list.foods$mddwgroup # make meaningful colnames
    mddwdat <- data.frame() # each column is an MDD-W food group
    for (i in 1:10) {
        if (i==1) {
            mddwdat <- data.frame(rowSums(tempdat[,colnames(tempdat)==i]))
        } else {
            if (sum(colnames(tempdat)==i)==1) {
                mddwdat <- cbind(mddwdat,(tempdat[,colnames(tempdat)==i]))
            } else {
                mddwdat <- cbind(mddwdat,rowSums(tempdat[,colnames(tempdat)==i]))
            }
        }
    }
    rm(tempdat)
    mddwdat[mddwdat>=1] <- 1
    colnames(mddwdat) <- paste("MDDW.group",sprintf("%02d",1:ncol(mddwdat)),sep=".")
    mddwdat$MDDWSUM <- rowSums(mddwdat) # how many MDD-W groups consumed?
    mddwdat$MDDW.5 <- (mddwdat$MDDWSUM>=5) # at least 5 consumed (Martin-Prevel et al 2017)
    mddwdat
}


# computeMDDC
# computes MDD-C for all children in "dhsdata", though it should only apply to children ages 6-23m
# returns data frame, one row for each DHS entry, indicating whether the individual has consumed each of the 8 food groups yesterday, the total number of food groups consumed, and an indicator if at least 5 groups were consumed
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
    
# compute MDD-C
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
    mddcdat$MDDCSUM <- rowSums(mddcdat) # how many MDD-C groups consumed?
    mddcdat$MDDC.5 <- (mddcdat$MDDCSUM>=5) # at least 5 consumed
    mddcdat
}
