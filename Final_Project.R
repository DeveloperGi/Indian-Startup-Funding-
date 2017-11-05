library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(DT)
library(stringr)
library(forcats)
Startup = read.csv(file.choose())

#To remove space between the column
# Collapse factor levels into manually different groups
#A series of named character vectors. The level in each vector will be replaced in name
Startup$InvestmentType=as.factor(Startup$InvestmentType)

Startup=Startup%>% mutate(InvestmentType=fct_collapse(InvestmentType,"PrivateEquity"=c("PrivateEquity","Private Equity"),
                                                 "SeedFunding"=c("SeedFunding","Seed Funding")))
View(Startup)


#To change the date 

Startup$date <- as.POSIXct(strptime(Startup$Date, format = "%d/%m/%Y"))
Startup$date
Startup$day <- as.integer(format(Startup$date, "%d")) # day
Startup$month <- as.factor(format(Startup$date, "%m")) # month
Startup$year <- as.integer(format(Startup$date, "%y")) # day
Startup$monthyear <- paste(Startup$year,Startup$month,sep="_")# year month
Startup



Startup %>%
  group_by(monthyear)%>%
  summarise(n = n())%>%
  drop_na(monthyear)%>%
  ggplot(aes(x =monthyear, y =  n )) +
  geom_bar(stat='identity',colour="white", fill = c("red"))+
  labs(x = 'Year_ month', y = 'Number of Startups Funded', title = 'Year-Month wise Startups Funding') +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))





