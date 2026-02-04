### Contracepting as a Social Norm ###
setwd("C:/Users/michelleob/OneDrive - Bill & Melinda Gates Foundation/Documents/DHS_Nov24")

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("idhs_00001.xml")
data <- read_ipums_micro(ddi)

## var recoding ##

data$fp_approve <- NA
data$fp_approve[data$FPAPPROVE == 0] <- "Does not approve"
data$fp_approve[data$FPAPPROVE == 1] <- "Approves"
data$fp_approve[data$FPAPPROVE == 3 | 
                  data$FPAPPROVE == 7] <- "Does not approve"
