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

life_history_data_long <- read_xlsx("data/Final Project Data.xlsx")
life_history_data_long <- read_sav("data/Final Project Data_long.sav")

# take a look at variables with colnames, str, and summary
colnames(life_history_data_long)
str(life_history_data_long) # variable types
summary(life_history_data_long)

# no factors, if we want to use any of these categorical vars like sex, gender, etc.
# need to be turned into factors and effect coded


