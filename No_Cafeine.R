# Caffeine 

library(readr)

##
## caf <- read_csv("oura.csv", col_types = cols(Cafeine = col_integer(), 
##                                              date = col_date(format = "%Y-%m-%d"), 
##  `Week Day` = col_integer()))

caf <- read.csv("oura.csv")
View(caf)

caf$Caf <- as.factor(ifelse(caf$Caffeine == 0, "No", "Yes"))

library(dplyr)


grpDat <- group_by(caf, Caf)
sum.tab <- summarize(grpDat, 
          mean.Deep.Sleep = mean(Deep.Sleep.Dur),
          mean.Total.Sleep = mean(Total.Sleep.Dur),
          meanLoRHR = mean(Lo.RHR),
          mean.AvHRV = mean(Avg.HRV),
          mean.Restless.Sleep = mean(Restless.Sleep))

library(kableExtra)
sum.tab %>%
  kbl() %>%
  kable_paper(full_width = F)   %>% 
  kable_styling()

summary(caf$Total.Sleep.Dur/3600)
summary(caf$Deep.Sleep.Dur/3600)
hist(caf$Deep.Sleep.Dur, breaks = 5)
hist(caf$Total.Sleep.Dur)

# https://intro2r.com/summarising-data-frames.html

par(mfrow = c(2, 2))
boxplot(Deep.Sleep.Dur ~ Caf, data = caf)
stripchart(Deep.Sleep.Dur ~ Caf, data = caf, method = "jitter", vertical = TRUE,
           pch = 19, add = TRUE)

boxplot(Total.Sleep.Dur ~ Caf, data = caf)
stripchart(Total.Sleep.Dur ~ Caf, data = caf, method = "jitter", vertical = TRUE,
           pch = 19, add = TRUE)


boxplot(Restfulness.Score ~ Caf, data = caf)
stripchart(Restfulness.Score ~ Caf, data = caf, method = "jitter", vertical = TRUE,
           pch = 19, add = TRUE)


boxplot(Avg.HRV ~ Caf, data = caf)
stripchart(Avg.HRV ~ Caf, data = caf, method = "jitter", vertical = TRUE,
           pch = 19, add = TRUE)

par(mfrow = c(1, 1))



# Let's try fisher test
# H0: The two variables are independent.
# H1: The two variables relate to each other.

################
#Deep Sleep
caf$DeepSleep.Stat <-as.factor(ifelse(caf$Deep.Sleep.Dur > mean(caf$Deep.Sleep.Dur), "more", "less"))
tab.Deep.Sleep <- table(caf$Caf, caf$DeepSleep.Stat)
names(dimnames(tab.Deep.Sleep)) <- c("Cafeine", "Deep Sleep")
tab.Deep.Sleep
fisher.test(tab.Deep.Sleep)
# We can NOT reject H0 - the variables might be independent

################
# Total Sleep

caf$TotSleep.Stat <-as.factor(ifelse(caf$Total.Sleep.Dur > mean(caf$Total.Sleep.Dur), "more", "less"))
tab.Tot.Sleep <- table(caf$Caf, caf$TotSleep.Stat)
names(dimnames(tab.Tot.Sleep)) <- c("Cafeine", "Tot Sleep ")
tab.Tot.Sleep
fisher.test(tab.Tot.Sleep)

boxplot(Total.Sleep.Dur ~ Caf, data = caf)
stripchart(Total.Sleep.Dur ~ Caf, data = caf, method = "jitter", vertical = TRUE,
           pch = 19, add = TRUE)


# We can NOT reject H0 - the variables might be independent
# 

################
# Restlessness
caf$Rest.Stat <-as.factor(ifelse(caf$Restfulness.Score > mean(caf$Restfulness.Score), "more", "less"))
tab.Restlessness <- table(caf$Caf, caf$Rest.Stat)
names(dimnames(tab.Restlessness)) <- c("Cafeine", "Restfulness.Score")
tab.Restlessness
fisher.test(tab.Restlessness)
# We can NOT reject H0 - the variables might be independent


