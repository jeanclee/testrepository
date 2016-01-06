install.packages("lubridate")
library(lubridate)
library(dplyr)

getwd()
setwd("\\\\AFSC-S79/REFM_Users/jean.lee/My Documents/Personal/datascience/birthdays")


dates <- read.csv("birthdaysExample.csv")

dates_pretty <- strptime(dates$dates,"%m/%d/%y")
dates_pretty

dates_noyear <- format(dates_pretty, format="%m-%d")
dates_noyear

dates_month <- format(dates_pretty, format="%m")
dates_month

dates_date <- format(dates_pretty, format="%d")
dates_date

dates_df <- as.data.frame(dates_pretty, stringsAsFactors=FALSE)
dates_df <- cbind(dates_df, dates_noyear, dates_month, dates_date)

str(dates_df)


### count of birthdays per month, sorted by number of birthdays
count(dates_df, dates_month) %>%
  arrange(-n)

as.numeric(dates_df$dates_month)

qplot(x=as.numeric(dates_df$dates_month), 
      binwidth=1,  
      color=I('black'), 
      fill=I('#F79420'),
      xlab="Month",
      ylab="Number of birthdays",
      main="Number of birthdays by month") +  
  scale_x_continuous(limits = c(1,13), breaks = seq(1, 12, 1))

ggsave("birthdays_bymonth.png")
      

## count of birthdays per day of month, sorted by number of birthdays
count(dates_df, dates_date) %>%
  arrange(-n)

qplot(x=as.numeric(dates_df$dates_date), 
      binwidth=1,  
      color=I('black'), 
      fill=I('#F79420'),
      xlab="Day of month",
      ylab="Number of birthdays",
      main="Number of birthdays by day of month") +  
  scale_x_continuous(limits = c(1,32), breaks = seq(0, 31, 2))

ggsave("birthdays_byday.png")


