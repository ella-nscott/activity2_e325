#install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)

streamH <- read.csv("/cloud/project/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/site_info.csv")

#parse our date (prompt 2)
streamH$dateF <- ymd_hm(streamH$datetime,
                        tz="America/New_York")
year(streamH$dateF)

#join site info to stream gauge height (prompt 1- continued further in script)
floods <- full_join(streamH, siteinfo, by="siteID")

peace <- floods %>%
  filter(siteID == 2295637)

example <- floods %>%
  filter(gheight.ft >= 10)

plot(peace$dateF,peace$gheight.ft, type="l")

max_ht <- floods %>%
  group_by(names) %>%
  summarise(max_ht_ft=max(gheight.ft, na.rm=TRUE),
            mean_ft=mean(gheight.ft, na.rm=TRUE))

#prompt 3
floodStage <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))


#Homework part 1 practice
help(ncol)

ncol(floods)
nrow(floods)
help(which)

which(max_ht$max_ht_ft > 20)

which(siteinfo$action.ft == 5)

which(max_ht$max_ht_ft == 15.45)

floods(nrow(floods)-2:nrow(floods))
floods$gheight.ft[nrow(floods):nrow(floods)]

example <- max_ht$max_ht_ft[(nrow(max_ht)-1):(nrow(max_ht))]


example2 <- siteinfo$flood.ft[(nrow(siteinfo)-1):(nrow(siteinfo))]

#use select to create df with fewer variables
simple_floods <- floods %>%
  select(-c('agency', 'siteID', 'datetime','moderate.ft','action.ft','major.ft'))

mutate_floods <- simple_floods %>%
  mutate(gheight.m = gheight.ft * 0.3048,
         flood.m = flood.ft * 0.3048)

#prompt 1- joins

floods <- full_join(streamH, siteinfo, by="siteID")
leftjoin <- left_join(streamH, siteinfo, by="siteID")
innerjoin <- inner_join(streamH, siteinfo, by="siteID")

#Homework 2 part 2
#Question 1

fisheating <- floods %>%
  filter(siteID == 2256500)
plot(fisheating$dateF, fisheating$gheight.ft, type = "l", xlab = "date", 
     ylab = "gauge height (ft)", main = "Stream height over time: Fisheating Creek")

santafe <- floods %>%
  filter(siteID == 2322500)
plot(santafe$dateF, santafe$gheight.ft, type = "l", xlab = "date",
     ylab = "gauge height (ft)", main = "Stream height over time: Santa Fe River")

peace <- floods %>%
  filter(siteID == 2295637)
plot(peace$dateF,peace$gheight.ft, type="l", xlab = "date",
     ylab = "gauge height (ft)", main = "Stream height over time: Peace River")

withla <- floods %>%
  filter(siteID == 2312000)
plot(withla$dateF, withla$gheight.ft, type="l", xlab = "date",
     ylab="gauge height (ft)", main= "Stream height over time: Withlacoochee River")

#Question 2
#find first and last occurances of each flood stage
actionStage <- floods %>%
  filter(gheight.ft >= action.ft) %>%
  group_by(names) %>%
  summarise (min_date = min(dateF))

moderateStage <- floods %>%
  filter(gheight.ft >= moderate.ft) %>%
  group_by(names) %>%
  summarise (min_date = min(dateF))

majorStage <- floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise (min_date = min(dateF))

floodStage <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

#find time between each flood stage

#action stage to flood stage in hours
(floodStage$min_date - actionStage$min_date)/3600  

#flood stage to moderate stage in hours
moderateStage$min_date - floodStage$min_date

#moderate stage to major stage in hours
majorStage$min_date - moderateStage$min_date

#Question 3:

#isolate the rows where stream stage is above height listed in major flood category
highestStream <- floods %>%
  filter(gheight.ft>=major.ft) %>%
  group_by(names) %>%
  summarise(maxheight = max(gheight.ft))


#join with site info to compare with defined major flood height
add_max <- full_join(highestStream, siteinfo)


#simplify table for easier comparison
heightChange <- add_max %>%
  select(-c("siteID", "action.ft", "flood.ft", "moderate.ft"))

#subtract columns to find change
heightChange$maxheight - heightChange$major.ft


