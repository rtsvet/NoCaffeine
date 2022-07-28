# Caffeine 

library(readr)
oura <- read_csv("oura.csv", col_types = cols(Cafeine = col_integer(), 
                                              date = col_date(format = "%Y-%m-%d"), 
                                              `Week Day` = col_integer()))
View(oura)


