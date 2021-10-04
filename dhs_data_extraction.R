#Multi-country DHS extraction

rm(list = ls()) ##clean your environment (important so that you don't accidentally bind unrelated dataframes later)

require(readstata13)
require(plyr)
require(dplyr)
require(gdata)

setwd("C:/Users/mobrien/OneDrive - Institute for Disease Modeling/Documents/DHS/ind_recodes")

varlist <- c("v000", #country-survey code
             "v001", #cluster
             "v005", #ind weights
             "v012", #respondent age
             #child alive 
             "b5_01", "b5_02", "b5_03", "b5_04", "b5_05", "b5_06", "b5_07", "b5_08", "b5_09", "b5_10", "b5_11", "b5_12", "b5_13", "b5_14", "b5_15", "b5_16", "b5_17", "b5_18", "b5_19", "b5_20",
             #age at death (days)
             "b6_01", "b6_02", "b6_03", "b6_04", "b6_05", "b6_06", "b6_07", "b6_08", "b6_09", "b6_10", "b6_11", "b6_12", "b6_13", "b6_14", "b6_15", "b6_16", "b6_17", "b6_18", "b6_19", "b6_20",
             #age at death (months)
             "b7_01", "b7_02", "b7_03", "b7_04", "b7_05", "b7_06", "b7_07", "b7_08", "b7_09", "b7_10", "b7_11", "b7_12", "b7_13", "b7_14", "b7_15", "b7_16", "b7_17", "b7_18", "b7_19", "b7_20",
             #preceding birth interval
             "b11_01", "b11_02", "b11_03", "b11_04", "b11_05", "b11_06", "b11_07", "b11_08", "b11_09", "b11_10", "b11_11", "b11_12", "b11_13", "b11_14", "b11_15", "b11_16", "b11_17", "b11_18", "b11_19", "b11_20",
             #succeeding birth interval
             "b12_01", "b12_02", "b12_03", "b12_04", "b12_05", "b12_06", "b12_07", "b12_08", "b12_09", "b12_10", "b12_11", "b12_12", "b12_13", "b12_14", "b12_15", "b12_16", "b12_17", "b12_18", "b12_19", "b12_20",
             #breastfeeding duration -- need to use KR instead of IR files
             #"m5_01", "m5_02", "m5_03", "m5_04", "m5_05", "m5_06", "m5_07", "m5_08", "m5_09", "m5_10", "m5_11", "m5_12", "m5_13", "m5_14", "m5_15", "m5_16", "m5_17", "m5_18", "m5_19", "m5_20",
             "v206", "v207", #sons/daughters alive
             "v212", #age at first birth
             #"v190", #wealth index
             #"v155", #literacy
             "v140", #urban-rural
             "v149", #educational attainment
             "v228", #terminated pregnancy ever
             "v301", #knowledge of any method
             "v624", #unmet need def 1
             "v626" #unmet need def 2
             )

temp = list.files(pattern="*.DTA") #list all files in the working directory with extension

for (i in 1:length(temp)) assign(temp[i], read.dta13(temp[i], select.cols = varlist))  #bring in all files in folder with selected variables

dfs = sapply(.GlobalEnv, is.data.frame) #list all dataframes in the environment

dhs <- do.call(rbind.fill, mget(names(dfs)[dfs])) #bind all the dataframes in environment, missing columns filled in

keep(dhs, sure = T) #remove all datasets except dhs, to preserve memory/space

dhs$country_code <- gsub("[[:digit:]]","",dhs$v000)
dhs$survey_round <- gsub("[^[:digit:]]", "", dhs$v000)

save(dhs, file = "dhs.csv")
