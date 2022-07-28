# Caffeine 

library(readr)

##
## caf <- read_csv("oura.csv", col_types = cols(Cafeine = col_integer(), 
##                                              date = col_date(format = "%Y-%m-%d"), 
##  `Week Day` = col_integer()))

caf <- read.csv("oura.csv")
View(caf)

caf$Caffeine.Fact <- as.factor(ifelse(caf$Caffeine == 0, "No", "Yes"))

library(report)
library(dplyr)
library("ggplot2")
library("GGally")

grpDat <- group_by(caf, Caffeine.Fact)
summarize(grpDat, 
          meanDeep.Sleep = mean(Deep.Sleep.Duration),
          meain.Total.Sleep = mean(Total.Sleep.Duration),
          meanLoRHR = mean(Lowest.Resting.Heart.Rate),
          meanAvHRV = mean(Average.HRV))

# Let's try fisher test
# H0: The two variables are independent.
# H1: The two variables relate to each other.

caf$DeepSleep.Stat <-as.factor(ifelse(caf$Deep.Sleep.Duration > mean(caf$Deep.Sleep.Duration), "inc", "dec"))
caf$TotSleep.Stat <-as.factor(ifelse(caf$Total.Sleep.Duration > mean(caf$Total.Sleep.Duration), "inc", "dec"))

tab.Deep.Sleep <- table(caf$Caffeine.Fact, caf$DeepSleep.Stat)
names(dimnames(tab.Deep.Sleep)) <- c("Cafeine", "Deep Sleep ")
tab.Deep.Sleep
kable(tab.Deep.Sleep,  )
fisher.test(tab.Deep.Sleep)


tab.Tot.Sleep <- table(caf$Caffeine, caf$TotSleep.Stat)
names(dimnames(tab.Deep.Sleep)) <- c("Cafeine", "Tot Sleep ")
tab.Tot.Sleep
fisher.test(tab.Tot.Sleep)

# We can NOT reject H0 - the variables might be independent
# 