## 958 final project
## R script to clean data and run analyses
## Luke Watson 
## Jacob Wilson
## 
## init: 10/16/24



# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) # read xlsx files
library(haven)

# load and view data ------------------------------------------------------

LHS_wide <- read_xlsx("data/Final Project Data_wide.xlsx")
LHS_long <- read_xlsx("data/Final Project Data_long.xlsx")

# take a look at variables with colnames, str, and summary
colnames(LHS_long)
str(LHS_long) # variable types
summary(LHS_long)

# no factors, if we want to use any of these categorical vars like sex, gender, etc.
# need to be turned into factors and effect coded


