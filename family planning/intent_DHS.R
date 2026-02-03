##Quick DHS patterns for Future Use Intentionsr
require(ggplot2)
require(questionr)
require(dplyr)

dat <- read.csv("C:/Users/michelleob/Downloads/intentions_DHS_priorityGeos.csv")
dat00 <- subset(dat, Survey.Year>=2000)

ggplot(data=subset(dat00, !is.na(Value)), 
       aes(x = Survey.Year, y=Value, 
          group=Indicator, colour=Indicator)) + 
  geom_line() + geom_point()+
  facet_wrap(~ Country.Name) + 
  xlab("") + ylab("Percent married women") + 
  theme(legend.title=element_blank()) + theme_bw()

geo_dat <- subset(dhs, country_code == "CI" | country_code == "PK" | 
                    (country_code == "NG" & v024 == "north west") | 
                    (country_code == "NG" & v024 == "north east") | 
                    (country_code == "NG" & v024 == "north central"))

geo_dat$year <- geo_dat$v007
geo_dat <- subset(geo_dat, geo_dat$year >= 2010)

geo_dat$intent <- geo_dat$v364
geo_dat$wt <- geo_dat$v005/1000000

## Separate country files if needed
CI_dat <- subset(geo_dat, country_code == "CI")
CI_dat$country_lab <- "Cote d'Ivoire"
NG_dat <- subset(geo_dat, country_code == "NG")
NG_dat$country_lab <- "Northern Nigeria"
PK_dat <- subset(geo_dat, country_code == "PK")
PK_dat$country_lab <- "Pakistan"

data_temp = NG_dat

dhs_fp_intent_prop_year_wt <- data_temp %>%
  dplyr::group_by(year, intent) %>%
  dplyr::mutate(count_wt = sum(wt, na.rm=TRUE),
                prop = sum(wt * (value == "Yes"), na.rm=TRUE)/sum(wt, na.rm=TRUE),
                variance = sum(wt^2 * ((value == "Yes") - prop)^2, na.rm=TRUE) / (sum(wt)^2 - sum(wt^2)),
                se = sqrt(variance),
                ci_upper = round(prop + (1.96 * se), 3),
                ci_lower = round(prop - (1.96 * se), 3)) %>%
  # dplyr::filter(value == "Yes") %>%
  dplyr::select(iso_code, survey, survey_year, admin1, wealth_index_str, agegroup, indicator, value, count_wt, prop, variance, se, ci_upper, ci_lower) %>%
  distinct() %>%
  ungroup()

intent_prop <- as.data.frame(wtd.table(x = data_temp$year, y = data_temp$intent, weights = data_temp$wt) %>% prop.table(margin=1))
ggplot(subset(intent_prop, Freq > 0.000), 
       aes(x=Var1, y=Freq, group=Var2, colour = Var2, label=Freq)) + 
  geom_line() + 
  ggtitle(data_temp$country_lab, "Trends in FP use and intention among married women") +
  xlab( "survey year") + ylab("% married women") + theme_bw() + 
  guides(colour=guide_legend("future intention or current use"))
