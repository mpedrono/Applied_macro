setwd("C:/Users/maily/OneDrive/Documents/Cours 3A ENSAE/Applied Macro")

library(tidyr)
library(dplyr)
library("ioanalysis")

load("WIOTS_in_R/WIOT2014_October16_ROW.RData")

wiot = data.frame(wiot)
# Re-index the row names in the same way as columns
wiot <- wiot %>% mutate(Code = paste(Country, RNr, sep=""))

#Define matrices

# Column 1-5 are row descriptions, the countries' data end at col 2469, and 2690 is the total output
# Row 2470 is value added
# Therefore, let's subset the data into our matrices.

unwanted_cols <- c("IndustryCode", "IndustryDescription", "Country", "RNr", "Year", "TOT")
# Inter-industry matrix
Z <- wiot %>% slice(1:2464) %>% select(Code, 6:2469)
# Total Output
x <- wiot %>% select(c(Code,TOT)) %>% slice(1:2464) 
# Value-add
v <- wiot %>% slice(2470) %>% select(Code, 6:2469)
# HH consumption for France
c <-  wiot %>% select(Code, FRA57) %>% slice(1:2464)

#code = https://gist.github.com/jasonrwang/fed65b0766b442ae4dee8ed3efddf6a9

