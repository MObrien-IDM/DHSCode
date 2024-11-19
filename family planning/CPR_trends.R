## DHS CPR growth analysis ##

#source(dhs_data_extraction.R)

table(dhs$v302)

dhs$ever_FP <- NA
dhs$ever_FP[dhs$v302 == "never used"] <- "never used"
dhs$ever_FP[dhs$v302 == "used only folkloric" | dhs$v302 == "used only folkloric method" |
              dhs$v302 == "used only trad. meth" | dhs$v302 == "used only traditional method" |
              dhs$v302 == "used any method"] <- "used folk/trad method"
dhs$ever_FP[dhs$v302 == "used modern method"] <- "used modern method"


dhs$ever_FP_v2 <- NA
dhs$ever_FP_v2[dhs$v302a == 0 | 
                 dhs$v302a == "no"] <- "Never used"
dhs$ever_FP_v2[dhs$v302a == 1 | 
                 dhs$v302a == "yes" |
                 dhs$v302a == "yes, currently use" |
                 dhs$v302a == "yes, no calendar data" |
                 dhs$v302a == "yes, used in calendar" |
                 dhs$v302a == "yes, used in the past" | 
                 dhs$v302a == "yes, used outside calendar"] <- "Ever used any method"

dhs$ever_FP_v3 <- NA
dhs$ever_FP_v3[dhs$v302a == 0 | 
                 dhs$v302a == "no" | 
                 dhs$v302 == "never used"] <- "Never used"
dhs$ever_FP_v3[dhs$v302a == 1 | 
                 dhs$v302a == "yes" |
                 dhs$v302a == "yes, currently use" |
                 dhs$v302a == "yes, no calendar data" |
                 dhs$v302a == "yes, used in calendar" |
                 dhs$v302a == "yes, used in the past" | 
                 dhs$v302a == "yes, used outside calendar" |
                 dhs$v302 == "used only folkloric" | 
                 dhs$v302 == "used only folkloric method" |
                 dhs$v302 == "used only trad. meth" | 
                 dhs$v302 == "used only traditional method" |
                 dhs$v302 == "used any method" |
                 dhs$v302 == "used modern method"
                 ] <- "Ever used any method"
table(dhs$v000, dhs$ever_FP_v3)
