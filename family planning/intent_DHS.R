##Quick DHS patterns for Future Use Intentions
require(ggplot2)
require(questionr)
require(dplyr)
require(freqtables)

## This section allows you to bring over statcompiler results in order to quickly observe trends over time by country. 'dat' should be read in from a .csv file you obtain from statcompiler. 
## Recommended practice is to download in database format, remove footnotes, and save as a .csv file

dat <- read.csv("C:/Users/michelleob/Downloads/intentions_DHS_priorityGeos.csv")
dat00 <- subset(dat, Survey.Year>=2000)

ggplot(data=subset(dat00, !is.na(Value)), 
       aes(x = Survey.Year, y=Value, 
          group=Indicator, colour=Indicator)) + 
  geom_line() + geom_point()+
  facet_wrap(~ Country.Name) + 
  xlab("") + ylab("Percent married women") + 
  theme(legend.title=element_blank()) + theme_bw()

#ALL GEOS
dat_global <- read.csv("C:/Users/michelleob/Downloads/DHS_intentions_all_geos.csv")
gdat00 <- subset(dat_global, Survey.Year>=2000)

ggplot(data=subset(gdat00, !is.na(Value)), 
       aes(x = Survey.Year, y=Value, 
           group=Indicator, colour=Indicator)) + 
  # geom_line() + 
  geom_point(aes(shape=Indicator))+
  facet_wrap(~ Country.Name) + 
  xlab("") + ylab("Percent married women") + 
  theme(legend.title=element_blank()) + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### This section plots weighted individual-level data for specific countries using DHS microdata which must be obtained at dhsprogram.com
### see recommended practice in the source file for data extraction
### source 'dhs_data_extraction.R' first

geo_dat <- subset(dhs, country_code == "CI" | country_code == "PK" | 
                    (country_code == "NG" & v024 == "north west") | 
                    (country_code == "NG" & v024 == "north east") | 
                    (country_code == "NG" & v024 == "north central"))

geo_dat$year <- geo_dat$v007
geo_dat <- subset(geo_dat, geo_dat$year >= 2010)
geo_dat$intent <- geo_dat$v364
geo_dat$wt <- geo_dat$v005/1000000

## Prep separate country files
vars_to_keep <- c("year", "intent", "wt", "country_code")
dat <- geo_dat[vars_to_keep]

## Separate country files if needed
CI_dat <- subset(dat, country_code == "CI")
CI_dat$country_lab <- "Cote d'Ivoire"
NG_dat <- subset(dat, country_code == "NG")
NG_dat$country_lab <- "Northern Nigeria"
PK_dat <- subset(dat, country_code == "PK")
PK_dat$country_lab <- "Pakistan"

## Re-use this code to visualize data from all three geographies
data_temp = CI_dat

## Build a weighted prop table from the subsetted data 
intent_prop <- as.data.frame(questionr::wtd.table(x = data_temp$year, y = data_temp$intent, weights = data_temp$wt) %>% prop.table(margin=1))

ggplot(subset(intent_prop, Freq > 0.000), #subsetting removes the artifact labels that may no longer be used
       aes(x=Var1, y=Freq, 
           group=Var2, 
           # colour = Var2, ## comment in if plotting a trend line
           fill = Var2, 
           label=Freq)) + 
  # geom_line() + ## Use this if you want a line
  geom_bar(stat="identity") +  #keep if you want a stacked bar 
  ggtitle(data_temp$country_lab, "Trends in FP use and intention among married women") +
  xlab( "survey year") + ylab("% married women") + theme_bw() + 
  guides(fill=guide_legend(title="future intention or current use")) + 
  scale_fill_brewer(palette = "Set1")  #fill is used for geom_bar() - comment out if not used
  #guides(colour=guide_legend(title="future intention or current use")) + 
  #scale_color_brewer(palette = "Set1") #color is used for geom_line() - comment out if not used
  
