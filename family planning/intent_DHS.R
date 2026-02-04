##Quick DHS patterns for Future Use Intentionsr
require(ggplot2)
require(questionr)
require(dplyr)
require(freqtables)


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

data_temp = CI_dat
##weighted 
intent_prop <- as.data.frame(questionr::wtd.table(x = data_temp$year, y = data_temp$intent, weights = data_temp$wt) %>% prop.table(margin=1))
# std.dev <- sqrt(Hmisc::wtd.var(intent_prop$Freq, wts))
# wtd.quantile(x, wts)

ggplot(subset(intent_prop, Freq > 0.000), 
       aes(x=Var1, y=Freq, 
           group=Var2, 
           # colour = Var2, 
           fill = Var2, 
           label=Freq)) + 
  # geom_line() + 
  geom_bar(stat="identity") +  #keep if you want a stacked bar 
  ggtitle(data_temp$country_lab, "Trends in FP use and intention among married women") +
  xlab( "survey year") + ylab("% married women") + theme_bw() + 
  guides(fill=guide_legend(title="future intention or current use")) + 
  scale_fill_brewer(palette = "Greens") 
  #guides(colour=guide_legend(title="future intention or current use")) + 
  #scale_color_brewer(palette = "Set1")
  

##with CIs
ci_prop <- data_temp %>% freq_table(intent, year)
ggplot(subset(ci_prop, percent_total > 0.000), 
       aes(x=col_cat, y=percent_total, group=row_cat, colour = row_cat)) + 
  geom_line() + 
  ggtitle(data_temp$country_lab, "Trends in FP use and intention among married women") +
  xlab( "survey year") + ylab("% married women") + theme_bw() + 
  guides(colour=guide_legend("future intention or current use"))

