library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

getwd()

### read in total fertility and regions data 

tf <-  read.csv("total_fertility.csv", header = TRUE, sep = ",")
str(tf)
View(tf)
tf <- rename(tf, Country=Total.fertility.rate)

regions <- read.csv("gapminder_regions.csv", header=TRUE, sep=",")
str(regions)
regions <- rename(regions, Country=Entity, Region=Group) 

## add regions to data; some country values manually coded
tf_regions <- left_join(tf, regions, by = "Country")
unique(filter(tf_regions, is.na(Region))$Country)
  
tf_regions$Region[tf_regions$Country %in% c("Cook Is", "South Korea", "North Korea", 
                                            "United Korea (former)", "Lao")] <- "East Asia & Pacific"
tf_regions$Region[tf_regions$Country %in% c("Central African Republic")] <- "Sub-Saharan Africa"
tf_regions$Region[tf_regions$Country %in% c("Falkland Is (Malvinas)", "St. Barthélemy",
                                            "St. Kitts and Nevis", "St. Martin", "St.-Pierre-et-Miquelon",
                                            "Dominican Republic", "St. Helena", "St. Lucia",
                                            "St. Vincent and the Grenadines")] <- "America"
tf_regions$Region[tf_regions$Country %in% c("North Yemen (former)", "South Yemen (former)", 
                                            "Yemen")] <- "Middle East & North Africa"
tf_regions$Region[tf_regions$Country %in% c("Czech Republic", "Kyrgyz Republic")] <- "Europe & Central Asia"

tf_regions <- select(tf_regions, -ID)



## melt dataset to get years as variable
tf_tidy <- melt(tf_regions, id=c("Country", "Region"))
tf_tidy <- tf_tidy %>% mutate(Year=substr(variable,2,5))
tf_tidy <- rename(tf_tidy, Fertility.Rate=value)
tf_tidy <- select(tf_tidy, -variable)
str(tf_tidy)

View(tf_tidy)

## changes in fertility rate, 1800 - 2015
by(tf_tidy$Fertility.Rate, tf_tidy$Year, summary)
summary(tf_tidy$Fertility.Rate)

qplot(x = Fertility.Rate, 
      data = subset(tf_tidy, Year %in% c("1800", "1900", "1950", "2000", "2015")), 
      binwidth = .5, geom='freqpoly', 
      color=Year,
      main="Fertility rate over time: 1800-2015",
      xlab = "Fertility rate (children per woman)",
      ylab="Number of countries") +
  scale_x_continuous(limits = c(0, 10),
                     breaks = seq(0, 10, .5)) +
  scale_y_continuous(breaks=seq(0,60,10))

ggsave("fertility_rate_over_time.png")

## 2015 fertility rates by region
qplot(x=Region, y=Fertility.Rate,
      data = subset(tf_tidy, Year=="2015"), 
      geom='boxplot',
      ylab="Fertility rate (children per woman)",
      main="Fertility rates by region, 2015") +
  scale_y_continuous(breaks=seq(0,9,0.5))

ggsave("fertility_rate_2015_by_region.png")

# changes in fertility rate, East Asia & Pacific
qplot(x=Year, y=Fertility.Rate,
      data = subset(tf_tidy, Region="East Asia & Pacific", Year %in% c("1800", "1900", "1950", "1960","1970","1980","1990","2000", "2010")), 
      geom='boxplot',
      ylab="Fertility rate (children per woman)",
      main="East Asia & Pacific fertility rate over time: 1800-2015") +
  scale_y_continuous(breaks=seq(0,9,0.5))

ggsave("fertility_rate_EAP_over_time.png")

save.image("gapminder_totalfertility.Rdata")



