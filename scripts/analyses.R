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
library(lme4)
library(MASS)

# load and view data ------------------------------------------------------

LHS_wide <- read_xlsx("data/Final Project Data_wide.xlsx")
LHS_long <- read_xlsx("data/Final Project Data_long.xlsx")
attach(LHS_long)

# take a look at variables with colnames, str, and summary
  colnames(LHS_long)
  str(LHS_long) # variable types
  summary(LHS_long)
  
# Cluster Analysis? -------------------------------------------------------


# no factors, if we want to use any of these categorical vars like sex, gender, etc.
# need to be turned into factors and effect coded


# Variables Checks-------------------------------------------------------
#Transformations
  log.av.in <- log(averagein)
  sqrt.av.in <- sqrt(averagein)
  log.dens <- log(dens)
  sqrt.SR <- sqrt(SR)
  sq.LE <- LE*LE
  DD <- DDk*1000
  logDD <- log(DDk)
  summary(DD)
#Income
  hist(averagein) #pretty bad
  hist(log.av.in)
  hist(sqrt.av.in)
    #shit bro, i guess sqrt?

#Density
  hist(dens) #oof
  hist(log.dens)
   #looks good

#Sex Ratio
  hist(SR) #not bad
  hist(sqrt.SR) #slightly better? may be unnecessary

#Life Expectancy
  hist(LE)
  hist(sq.LE) #not bad

#Mixed models had trouble converging, I tried normalizing to fix
  mean(sqrt.av.in, na.rm = T)
  sd(sqrt.av.in, na.rm = T)
  mean(log.dens, na.rm = T)
  sd(log.dens, na.rm = T)
  mean(sqrt.SR, na.rm = T)
  sd(sqrt.SR, na.rm = T)
  mean(sq.LE, na.rm = T)
  sd(sq.LE, na.rm = T)

  z.t.in <- (sqrt.av.in-241.8432)/103.3245
  z.t.dens <- (log.dens-5.74259)/2.335453
  z.t.SR <- (sqrt.SR-7.333837)/0.6604219
  z.t.LE <- (sq.LE-5515.567)/2013.024
# Linear Analysis ---------------------------------------------------------

#linear model, I think this is all we need
  badmodel <- lm(DDk~sqrt.av.in+log.dens+sqrt.SR+sq.LE, data=LHS_long)
    summary(badmodel)

  #Might be nice to have the mixed effects models of the linear analysis for comparison
      badmodelz <- lmer(DDk~z.t.in+z.t.dens+z.t.SR+z.t.LE+(1|ResponseId), data=LHS_long)
       summary(badmodelz)
          #t-values are equal to 0, not sure what's going on
        
# Generalized Linear Analysis ---------------------------------------------
  goodmodel <- glmer(DDk~sqrt.av.in+log.dens+sqrt.SR+sq.LE+(1|ResponseId), data=LHS_long, family=Gamma(link="log"))
    summary(goodmodel)
      #this also looks awful

  #Here I try to fix by normalizing predictors
    goodmodelz <- glmer(DDk~z.t.in+z.t.dens+z.t.SR+z.t.LE+(1|ResponseId), data=LHS_long, family=Gamma(link="log"))
      summary(goodmodelz)
        #didn't help
      
# Bayesian Analysis? ------------------------------------------------------


