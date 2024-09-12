#install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)

streamH <- read.csv("/cloud/project/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/site_info.csv")

#parse our date
streamH$dateF <- ymd_hm(streamH$datetime,
                        tz="America/New_York")
year(streamH$dateF)

#join site info to stream gauge height
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

floodStage <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))


