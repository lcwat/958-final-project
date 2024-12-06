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
LHS_weights <- read.csv("data/lin-weight-untransf-lhs-data.csv")
attach(LHS_weights)

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
#new variables
  hist(tw.in)
  hist(tw.hhin)
  hist(tw.roommates)
  hist(tw.dens)
  hist(tw.SR)
  hist(tw.LE)
# Linear Analysis ---------------------------------------------------------

#linear model, I think this is all we need
  #time-weighting
  lm_time_weighted <- lm(DDk~tw.in+tw.dens+tw.SR+tw.LE, data=LHS_wide)
    summary(lm_time_weighted)
  #primacy weighting
  lm_prim_weighted <- lm(DDk~prim_av_income+prim_density+prim_sex_ratio+prim_life_expct, data=LHS_weighted)
    summary(lm_prim_weighted)
  #recency weighting
  lm_rec_weighted <- lm(DDk~rec_av_income+rec_density+rec_sex_ratio+rec_life_expct, data=LHS_weighted)
    summary(lm_prim_weighted)
      
# Generalized Linear Analysis ---------------------------------------------
  #time-weighting
  glm_time_weighted <- glm(DDk~tw.in+tw.dens+tw.SR+tw.LE, data=LHS_wide, family=Gamma(link="log"))
    summary(glm_time_weighted)
  #primacy weighting
  glm_prim_weighted <- lm(DDk~prim_av_income+prim_density+prim_sex_ratio+prim_life_expct, data=LHS_weighted, family=Gamma(link="log"))
    summary(lm_prim_weighted)
  #recency weighting
  glm_rec_weighted <- lm(DDk~rec_av_income+rec_density+rec_sex_ratio+rec_life_expct, data=LHS_weighted, family=Gamma(link="log"))
    summary(lm_prim_weighted)
