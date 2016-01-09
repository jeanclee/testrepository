library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

getwd()

mort <-  read.csv("under5mortality.csv", header = TRUE, sep = ",")
str(mort)
colnames(mort)[1] <-"Country"

## melt dataset to get years as variable
mort_tidy <- melt(mort, id=c("Country"))
mort_tidy <- mort_tidy %>% mutate(Year=substr(variable,2,5))
mort_tidy <- rename(mort_tidy, Mortality.Rate=value)
mort_tidy <- select(mort_tidy, -variable)
str(mort_tidy)

View(mort_tidy)

### join in fertility data

load("gapminder_totalfertility.Rdata")
str(tf_tidy)

mort_fertility <- inner_join(mort_tidy, tf_tidy, by = c("Country","Year")) 
str(mort_fertility)

mort_fertility %>%
  filter(is.na(Region)) %>%
  select(Country)

View(mort_fertility)


## plot1: worldwide, 2000-present with linear model
ggplot(aes(x=Mortality.Rate, y=Fertility.Rate), data=subset(mort_fertility,Year>=2000)) + 
  geom_point(alpha=1/10) +
  xlim(0,250) +
  geom_smooth(method='lm',color='red') +
  xlab("Under 5 mortality rate (per 1000 live births)") +
  ylab("Fertility rate (children per woman)") +
  ggtitle("Child mortality rate vs. fertility rate, 2000-present")

ggsave("childmortality_fertility_2000.png")

##correlation
with(subset(mort_fertility, Year>=2000), cor.test(Mortality.Rate, Fertility.Rate, method='pearson'))
with(subset(mort_fertility, Year>=1800), cor.test(Mortality.Rate, Fertility.Rate, method='pearson'))


##plot 2: by region, 2000-present with linear model
ggplot(aes(x=Mortality.Rate, y=Fertility.Rate), data=subset(mort_fertility,Year>=2000)) + 
  geom_point(alpha=1/10) +
  facet_wrap(~Region) +
  xlim(0,250) +
  xlab("Under 5 mortality rate (per 1000 live births)") +
  ylab("Fertility rate (children per woman)") +
  ggtitle("Child mortality rate vs. fertility rate, 2000-present") +
  geom_smooth(method='lm',color='red') 

ggsave("childmortality_fertility_regional_2000.png")


##plot3: conditional means with smoothing

##plot3a-some smoothing

ggplot(aes(x=round(Mortality.Rate/5)*5, y=Fertility.Rate), data=subset(mort_fertility,Year>=2000)) +
  geom_line(stat='summary', fun.y=mean) +
  xlab ("Under 5 mortality rate per 1000 births (rounded to 5)") +
  ylab ("Mean fertility rate") +
  ggtitle ("Child mortality rate vs. fertility rate, 2000-present")

ggsave("childmortality_fertility_mean.png")

##plot3b - more smoothing

ggplot(aes(x=round(Mortality.Rate/10)*10, y=Fertility.Rate), data=subset(mort_fertility,Year>=2000)) +
  geom_line(stat='summary', fun.y=mean) +
  xlab ("Under 5 mortality rate per 1000 births (rounded to 10)") +
  ylab ("Mean fertility rate") +
  ggtitle ("Child mortality rate vs. fertility rate, 2000-present")

ggsave("childmortality_fertility_mean2.png")


