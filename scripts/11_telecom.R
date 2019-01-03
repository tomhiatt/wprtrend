# setwd("~/Dropbox/Current work space/STB_Mywork_2017/RD")

library(WDI) # extract world bank data
library(ggplot2)
library(reshape2)
library(dplyr)
library(grid)
library(gridExtra)
library(ggrepel)
library(scales)
library(xlsx)
library(treemap)
library(countrycode)
library(tidyr)
library(stringr)

options(stringsAsFactors = FALSE)

# telecom data
# ITU summary indicators
## Key ICT indicators for the ITU/BDT regions (totals and penetration rates)
## 2018 is estimated. 

## variables
ituc <- data.frame(
  name = c(
    "Fixed-telephone subscriptions", 
    "Mobile-cellular telephone subscriptions",
    "Active mobile-broadband subscriptions",
    "Fixed broadband subscriptions",
    "Households with a computer",
    "Households with Internet access at home",
    "Individuals using the Internet"), 
  code = c(
    "fixd.tel", 
    "mob.tel",
    "mob.brnd",
    "fixd.brnd",
    "hh.comp",
    "hh.internet",
    "indiv.internet"
  )
)

itua <- read.xlsx("data/Telecom/ITU_Key_2005-2018_ICT_data_with LDCs_rev27Nov2018.xls", 1, startRow = 56, rowIndex = c(56, 57:63, 64, 66:72, 74:80, 82:87, 91:98, 100:106, 108:113), colIndex = c(1, 16:29)) %>% rename(Region=NA.) %>% 
  mutate(var=c(
    rep(ituc$code[1],7),
    rep(ituc$code[2],7),
    rep(ituc$code[3],7),
    rep(ituc$code[4],7),
    rep(ituc$code[5],7),    
    rep(ituc$code[6],7),    
    rep(ituc$code[7],7)
  ))
  
   ### Test to see if coding is right 
itud <- itua %>% filter(is.na(X2018.)) %>% select(name=Region, code=var)
if (!identical(itud, ituc)) print("All is NOT well!")

itub <- itua %>% gather(yr, value, -Region, -var) %>% filter(!is.na(Region)) %>% transmute(Region, year=as.numeric(str_extract(yr, "[0-9]+")), var, value=as.numeric(value)) %>% filter(!is.na(value)) %>% spread(var, value)

## Plot
itub %>% select(Region, year, ituc$code[1:4]) %>% gather(code, value, -Region, -year) %>% inner_join(ituc) %>% ggplot(aes(year, value, color=Region, group=Region)) + facet_wrap(~name, scales = "free_y") + geom_line(size=1) + labs(x="", y="Per 100 inhabitants", title="Key ICT indicators for the ITU/BDT regions, 2005-2018", subtitle="* values for 2018 are estimated. \nCIS=Commonwealth of Independent States (formed when the former Soviet Union dissolved).")

itub %>% select(Region, year, ituc$code[5:7]) %>% gather(code, value, -Region, -year) %>% inner_join(ituc) %>% 
  ggplot(aes(year, value, color=Region, group=Region) ) + facet_wrap(~name, scales = "free_y", nrow=2) + geom_line(size=1) + labs(x="", y="%")



# Fixed broadband data
dta <- read.xlsx("data/Telecom/Fixed_broadband_2000-2017.xls", 1, startRow = 2, colIndex = c(1:19,21:38)) %>% rename(country=NA.) 

# Add in iso code
dta$iso3 <- countrycode(dta$country, 'country.name.en', 'iso3c')

## Fix countries countrycode can't handle
dta[is.na(dta$iso3),"country"]
dta[dta$country=="Central African Rep.", "iso3"] <- "CAF"
dta[dta$country=="Eswatini", "iso3"] <- "SWZ"
dta[dta$country=="Micronesia", "iso3"] <- "FSM"
dta[dta$country=="Neth. Antilles", "iso3"] <- "ANT"
dta[dta$country=="Ascension", "iso3"] <- "ASC"

# Add in WHO regions and remove non-WHO
# http://intranet.who.int/dev/refmartviewer/CustomViews/VIEW_CountryList
country.list <- read.csv("data/CountryList.csv")

dtb <- country.list %>% select(Title, iso3=ISO_Alpha3_Code_CODE, WHO_Region) %>% inner_join(dta) %>% select(-country, country=Title) %>% filter(WHO_Region!="")


# Reshape and calculate vars
dtc <- dtb %>% 
  gather(yr, value, -country, -iso3, -WHO_Region) %>% 
  mutate(var=if_else(str_detect(yr, "\\.1"), "fxbrnd.100", "fxbrnd"), year=as.numeric(str_extract(yr,"[0-9]+")), value=as.numeric(str_remove_all(value, "\'"))) %>% 
  select(-yr) %>% 
  spread(var, value) %>% 
  ## Calculate population used
  mutate(pop=fxbrnd/fxbrnd.100*100)

# Calculate globally and by region
# # missing data are excluded from numerator and denominators.
dtc %>% group_by(year, WHO_Region) %>% summarise(fxbrnd=sum(fxbrnd, na.rm=T), pop=sum(pop, na.rm=T)) %>% mutate(fxbrnd.100=fxbrnd/pop*100) -> reg.1
reg.1 %>% summarise(fxbrnd=sum(fxbrnd, na.rm=T), pop=sum(pop, na.rm=T)) %>% mutate(fxbrnd.100=fxbrnd/pop*100) -> reg.2
reg.1 %>% bind_rows(reg.2) %>% 
  ggplot(aes(year, fxbrnd.100, color=WHO_Region)) + geom_line()

dtc %>% filter(WHO_Region=="WPR", !is.na(fxbrnd.100)) %>% left_join() %>%  
  ggplot(aes(year, fxbrnd.100, group=country)) + facet_wrap(~country) + geom_line(size=1) + labs(x="", y="Per 100 inhabitants", title="Fixed-broadband subscriptions, 2000-2017") + geom_point(size=1)









