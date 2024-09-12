### Contraceptive Autonomy Indicators

#use DHS Data Extraction file to build a dataset called "dhs"

dat <- dhs %>% 
  mutate(
    reason_cat = case_when(
      (v376 == 11 | v376 == "not married" | v376 == "not intending to marry" | 
         v376 == "no sex" | v376 == "no sexual intercourse" | v376 == "infrequent sex" | 
         v376 == "don't have sex" | v376 == "long interv." | v376 == 22) ~ 
        "not at risk: behavioral",
      (v376 == 23 | v376 == 24 | v376 == "difficult to be preg" | v376 == "menopausal, had hyst" | 
         v376 == "menopausal, hyster." | v376 == "menopausal, hysterectomy" | 
         v376 == "post partum  / breastfeeding" | v376 == "post partum breastfeeding" | v376 == "sub-fecund, in-fecund" |
         v376 == "subfecund, infecund" | v376 =="too young /not menst") ~ 
        "not at risk: biological",
      (v376 == 41 | v376 == 42 | v376 == 53 | v376 == 54 | v376 == 55 | v376 == "cost too much" | v376 == "hard to get methods" |
         v376 == "inconvenient" | v376 == "inconvenient to use" | v376 == "lack of access") ~ 
        "supply-side failure",
      (v376 == "wants more children" | v376 == "wants children" | v376 == "wants as many children as possible" | 
         v376 ==26) ~ 
        "not in need: family building",
      (v376 == 41 | v376 == 42 | v376 == "knows no method" | v376 == "knows no source" | v376 == "knows no sources" | 
         v376 == "lack of information" | v376 == "lack of knowledge") ~
        "lack of information/knowledge",
      (v376 == 32 | v376 ==  33 | v376 == 34 | v376 == "cultural taboo" | v376 == "cultural taboos" | v376 == "cultural tabou {ci}"|
         v376 == "husband decides" | v376 == "husband opposed"| v376 == "other people opposed" | v376 ==  "others opposed" |
         v376 == "partner opposed" | v376 == "religious prohibit." | v376 == "religious prohibition" | v376 == "religion") ~
        "external opposition",
      (v376 == 31 | v376 == "respondent opposed" | v376 == "opposed to fp" | v376 == "fatalistic" | v376 == "fatalist" |
         v376 == "don't want to use" | v376 == "dislike") ~ 
        "intrinsic opposition/does not want to use",
      (v376 == 51 | v376 == 52 | v376 == 56 | v376 == "fear side effects" | v376 == "fears side effects" | v376 == "health concerns" |
         v376 == "interfere body proc." | v376 == "interfere with body" | v376 == "interferes with body" | v376 == "side effects") ~
        "health concerns",
      (v376 == 96 | v376 == 98 | v376 == "other" | v376 == "don't know" | v376 == "dk") ~ 
        "other"),
    int_date = 1900+(v008/12), ##cmc translation
    int_year_bin = case_when(
      int_date < 2000 ~ "1990s",
      (int_date>=2000 & int_date<2010) ~ "2000s",
      (int_date>=2010 & int_date<2020) ~ "2010s"
    )
    )



table(dat$v376, dat$reason_cat, useNA = "ifany")
table(dat$v007, dat$reason_cat)

require(ggplot2)
require(viridis)
ggplot(subset(dat, 
              is.na(reason_cat)==FALSE &  
                # reason_cat != "not at risk: biological" & 
                # reason_cat != "not at risk: behavioral" &
              reason_cat != "not in need: family building"), 
       aes(x=int_year_bin, 
           fill = as.factor(reason_cat))) + 
  geom_histogram(stat="count", position="dodge") +
  # geom_density(alpha=0.3) + 
  # facet_wrap(~ country_code) + 
  scale_fill_brewer(palette="Set2")
