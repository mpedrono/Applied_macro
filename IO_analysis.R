setwd("C:/Users/maily/OneDrive/Documents/Cours 3A ENSAE/Applied Macro")

library(tidyr)
library(dplyr)
library(ioanalysis)
library(readxl)

load("WIOTS_in_R/WIOT2014_October16_ROW.RData")

wiot = data.frame(wiot)
# Re-index the row names in the same way as columns
wiot <- wiot %>% mutate(Code = paste(Country, RNr, sep=""))

#co2 <- read_excel("CO2 emissions.xlsx")

###################
# Define matrices #
###################

# Column 1-5 are row descriptions, the countries' data end at col 2469, and 2690 is the total output
# Row 2470 is value added
# Therefore, let's subset the data into our matrices.

#nb countries
K <- n_distinct(wiot %>% filter(Country!="TOT") %>% select(Country))  #remove TOT (not a country)

#nb sectors
N <- n_distinct(wiot %>% filter(Country!="TOT") %>% select(IndustryCode))

unwanted_cols <- c("IndustryCode", "IndustryDescription", "Country", "RNr", "Year", "TOT")

# Inter-industry matrix
Z_df  <- wiot %>% filter(Country!="TOT") %>% select(Code, 6:2469)
Z <- as.matrix(Z_df %>% select(-Code))

# Total Output
x_df <- wiot %>% filter(Country!="TOT") %>% select(c(Code,TOT)) 
# Value-add
v_df <- wiot %>% filter(IndustryCode="VA") %>% select(Code, 6:2469)
# HH consumption for France
c_df <-  wiot %>%  filter(Country!="TOT") %>% select(Code, FRA57)


#Computation of the technical coefficients

x_hat <- matrix(0, K*N, K*N)
diag(x_hat) <- x$TOT

x_hat_inv <- matrix(0, K*N, K*N)
diag(x_hat_inv) <- 1/x$TOT
x_hat_inv[is.infinite(x_hat_inv)] <- 0 #remove infinite values

A <- Z %*% x_hat_inv

#Leontief matrix

I <- matrix(0, K*N, K*N)
diag(I) <- rep(1,K*N)
L = solve(I-A)


#code = https://gist.github.com/jasonrwang/fed65b0766b442ae4dee8ed3efddf6a9

