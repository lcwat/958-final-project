## 958 final project
## R script to clean data and run analyses
## Luke Watson 
## Jacob Wilson
## 
## init: 10/16/24
## 
## PLAN:
## 1. create informative averages of each of the life history variables based 
## on when the person lived in that location
##  a. early critical period hypothesis: earlier life experiences hold more 
##  weight than later, create weighted average that weights more recent exper-
##  iences lower
##  b. recency hypothesis: most recent life experience influences psychology 
##  more, create weighted average that weights more recent experiences higher
##  c. consider whether to transform the data before weighting or after 
##  
## 2. create two models, a simple linear model and a generalized linear model
## using gamma error distribution and a log link with all predictors for both
## hypotheses
##  a. linear model could utilize a log transformation and/or no transformation
##  to be a really shitty model
##  b. check assumptions using performance and go with the better model that 
##  matches those best (AIC will not be helpful)
##  c. report results (tables, figures, etc.)
##
## 3. to avoid overfitting given there will be about 10 or so predictors, 
## we will use k-fold cross validation for the preferred model to ensure that
## the model validates well within the dataset 
##  a. report results of robustness



# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) # read xlsx files
library(haven)
library(lme4)
library(MASS)

# load and view data ------------------------------------------------------

LHS_wide <- read_xlsx("data/Final Project Data_wide.xlsx")
LHS_long <- read_xlsx("data/Final Project Data_long.xlsx")
attach(LHS_wide)

# ggplot
LHS_long |> 
  ggplot(aes(x = averagein, y = DDk, color = as.factor(ResponseId))) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

LHS_long |> 
  ggplot(aes(x = averagein)) +
  geom_density()

# NOTES FROM MIKE:
# mike suggests reducing each subjects location/life history data down to 
# aggregate means for each predictor (income, density, sr, le), or if you want 
# to include all of those locations, could find a way to weight each of the 
# predictor estimates of each subject (like more recent experience being 
# weighted more or earlier experiences being weighted more)

# Suggestions for complex techniques we could use:
# b/c data is highly skewed, could use bootstrapping for regression weights 
# between adjusted and non-adjusted predictor models to see how the variability 
# in estimates change as outliers are excluded and excluded in the bootstrapped 
# samples 
# 
# use cross-validation to see how the model fits (encouraged to do at least one of
# cross validation or bootstrapping, one is fine)
# 
# 


# take a look at variables with colnames, str, and summary
  colnames(LHS_long)
  str(LHS_long) # variable types
  summary(LHS_long)
  
# Variables Checks-------------------------------------------------------
#Transformations
  log.av.in <- log(averagein)
  sqrt.av.in <- sqrt(averagein)
  log.dens <- log(dens)
  sqrt.SR <- sqrt(SR)
  sq.LE <- LE*LE
  logDD <- log(DDk)
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
       
      testmodel <- lmer(DDk~z.t.in+z.t.dens+z.t.SR+z.t.LE+(1|ResponseId:locationid), data=LHS_long)
        #this produces better estimates, so I think this might be the correct RE structure 
        #but it also introduces convergence issues.
      
# Generalized Linear Analysis ---------------------------------------------
  goodmodel <- glmer(DDk~sqrt.av.in+log.dens+sqrt.SR+sq.LE+(1|ResponseId), data=LHS_long, family=Gamma(link="log"))
    summary(goodmodel)
      #this also looks awful

  #Here I try to fix by normalizing predictors
    goodmodelz <- glmer(DDk~z.t.in+z.t.dens+z.t.SR+z.t.LE+(1|ResponseId), data=LHS_long, family=Gamma(link="log"))
      summary(goodmodelz)
        #didn't help

    testmodel2 <- glmer(DDk~z.t.in+z.t.dens+z.t.SR+z.t.LE+(1|ResponseId:locationid), data=LHS_long, family=Gamma(link="log"))
      summary(testmodel2)
        #looks better but convergence issues
