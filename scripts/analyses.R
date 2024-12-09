## 958 final project
## R script to clean data and run analyses
## Luke Watson 
## Jacob Wilson
## 
## init: 10/16/24
## 
## PLAN:
## 1. (done) create informative averages of each of the life history variables based 
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
##  
##  # NOTES FROM MIKE:
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



# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) # read xlsx files
library(boot) # cross validation and bootstrapping
library(cv) # cross validation
library(lme4) # glm
library(MASS)
library(performance) # assumption checks
library(emmeans)

# load and view data ------------------------------------------------------

## original files, uncomment to load in 
# LHS_long <- read_xlsx("data/Final Project Data_long.xlsx")

our_seed <- 958958

set.seed(our_seed)

# adjusted predictors file (untransformed)
LHS_wide <- read_xlsx("data/Final Project Data_wide.xlsx")
LHS_weights <- read_csv("data/lin-weight-untransf-lhs-data.csv")
attach(LHS_weights)

# prefixes: 
# 1. mean = unweighted aggregate score using simple mean
# 2. prim = weighted aggregate score favoring earlier life locations
# 3. rec = weighted aggregate score favoring later life locations
# 4. los = weighted aggregate score by length of stay in location
# 5. prim_los = weighted favoring earlier and longer residences
# 6. rec_los = weighted favoring later and longer residences

# visualize
LHS_weights |> 
  ggplot(aes(x = log(los_density), y = log(d_dk))) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "none")

# hist
LHS_long |> 
  ggplot(aes(x = averagein)) +
  geom_density()


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

  

# functions ---------------------------------------------------------------

# general function for bootstrapping regression parameters
# 1. pass in model type, either "lm" or "glm" (for Gamma(link = "log"))
# 2. pass in vector of names of parameters you want to include in model (
# additive, no interactions)
boot_param <- function(modeltype, formula, d, indices) {
  # take subset of the data
  data <- d[indices, ]
  
  # which model command should be used?
  if(modeltype == "lm") {
    return(
      coef(lm(formula, data = data))
    )
  } else if(modeltype == "glm"){
    return(
      coef(glm(
        formula,
        family = Gamma(link = "log"), # gamma error with log link
        data = data
      ))
    )
  } else {
    stop("Hey buddy, specify model type within quotes")
  }
}
  
# Linear Analysis ---------------------------------------------------------

  #unweighted
  lm_agg_unweighted <- lm(d_dk ~ mean_av_income + mean_density + mean_sex_ratio + mean_life_expct, 
    data = LHS_weights)
      summary(lm_agg_unweighted)
  
  # bootstrap regression weights
  unweighted_boot <- boot(LHS_weights, statistic = boot_param, R = 2500, modeltype = "lm",
    formula = d_dk ~ mean_av_income + mean_density + mean_sex_ratio + mean_life_expct)
  
  # boot seems to suggest that effects of sex ratio and le are significant in 
  # opposition to the model summary (index 4 and 5)
  quantile(unweighted_boot$t[,5], probs = c(.025, .975))
  
  # view hist of bootstrapped values
  hist(unweighted_boot$t[, 5])
  
  #time-weighting
  lm_time_weighted <- lm(DDk ~ tw.in + tw.dens + tw.SR + tw.LE, data=LHS_wide)
  summary(lm_time_weighted)
    # bootstrap regression weights
      timeweighted_boot <- boot(LHS_weights, statistic = boot_param, R = 2500, modeltype = "lm",
      formula = d_dk ~ tw_in + tw_dens + tw_sr + tw_le)
    # view bootstrapped values
      quantile(timeweighted_boot$t[,5], probs = c(.025, .975))
      hist(timeweighted_boot$t[, 5])
    
  #primacy weighting
  lm_prim_weighted <- lm(d_dk ~ prim_av_income + prim_density + prim_sex_ratio + prim_life_expct, 
    data = LHS_weights)
      summary(lm_prim_weighted)
            
  #with time
    lm_primlos_weighted <- lm(d_dk ~ prim_los_av_income + prim_los_density + prim_los_sex_ratio + prim_los_life_expct, 
      data = LHS_weights)
        summary(lm_primlos_weighted)
              
  #recency weighting
      lm_rec_weighted <- lm(d_dk~rec_av_income+rec_density+rec_sex_ratio+rec_life_expct, data=LHS_weights)
        summary(lm_rec_weighted)
            
      #with time
        lm_reclos_weighted <- lm(d_dk~rec_los_av_income+rec_los_density+rec_los_sex_ratio+rec_los_life_expct, data=LHS_weights)
          summary(lm_reclos_weighted)
          check_model(lm_reclos_weighted)
          
          lm_reclos_weighted_simple <- lm(d_dk~rec_los_av_income, data=LHS_weights)
          summary(lm_reclos_weighted_simple)

library(performance)
# Generalized Linear Analysis ---------------------------------------------
              
  #unweighted
      glm_agg_unweighted <- glm(d_dk ~ mean_av_income + mean_density + mean_sex_ratio + mean_life_expct, data = LHS_weights, family=Gamma(link="log"))
        summary(glm_agg_unweighted)

  #time-weighting
      glm_time_weighted <- glm(d_dk~tw_in+tw_dens+tw_sr+tw_le, data=LHS_weights, family=Gamma(link="log"))
        summary(glm_time_weighted)
            
    
  #primacy weighting
      glm_prim_weighted <- glm(d_dk~prim_av_income+prim_density+prim_sex_ratio+prim_life_expct, data=LHS_weights, family=Gamma(link="log"))
        summary(lm_prim_weighted)
            
      #with time
        glm_primlos_weighted <- glm(d_dk~prim_los_av_income+prim_los_density+prim_los_sex_ratio+prim_los_life_expct, data=LHS_weights, family=Gamma(link="log"))
          summary(lm_primlos_weighted)
              
  #recency weighting
      glm_rec_weighted <- glm(d_dk~rec_av_income+rec_density+rec_sex_ratio+rec_life_expct, data=LHS_weights, family=Gamma(link="log"))
        summary(lm_rec_weighted)
          
      #With time
        glm_reclos_weighted <- glm(d_dk~rec_los_av_income+rec_los_density+rec_los_sex_ratio+rec_los_life_expct, data=LHS_weights, family=Gamma(link="log"))
          summary(glm_reclos_weighted)


# Model Comparisons -------------------------------------------------------
  summary(lm_agg_unweighted)
  summary(lm_time_weighted)
  summary(lm_prim_weighted)
  summary(lm_primlos_weighted)
  summary(lm_rec_weighted)
  summary(lm_reclos_weighted)
  
  summary(glm_agg_unweighted)
  summary(glm_time_weighted)
  summary(glm_prim_weighted)
  summary(glm_primlos_weighted)
  summary(glm_rec_weighted)
  summary(glm_reclos_weighted)
  
  AIC(lm_agg_unweighted)
  AIC(lm_time_weighted)
  AIC(lm_prim_weighted)
  AIC(lm_primlos_weighted)
  AIC(lm_rec_weighted)
  AIC(lm_reclos_weighted)
  
  AIC(glm_agg_unweighted)
  AIC(glm_time_weighted)
  AIC(glm_prim_weighted)
  AIC(glm_primlos_weighted)
  AIC(glm_rec_weighted)
  AIC(glm_reclos_weighted)       

  cv(lm_agg_unweighted, seed = our_seed)
  cv(lm_time_weighted, seed = our_seed)
  cv(lm_prim_weighted, seed = our_seed)
  cv(lm_primlos_weighted, seed = our_seed)
  cv(lm_rec_weighted, seed = our_seed)
  cv(lm_reclos_weighted, seed = our_seed)
  
  cv(glm_agg_unweighted, seed = our_seed)
  cv(glm_time_weighted, seed = our_seed)
  cv(glm_prim_weighted, seed = our_seed)
  cv(glm_primlos_weighted, seed = our_seed)
  cv(glm_rec_weighted, seed = our_seed)
  cv(glm_reclos_weighted, seed = our_seed) 
  

# Plots -------------------------------------------------------------------
  lmreclosplot = data.frame(emmeans(lm_reclos_weighted, ~rec_los_av_income+rec_los_density+rec_los_sex_ratio+rec_los_life_expct,
                             at=list(rec_los_av_income=seq(0, 500000, by = 5000))))
 
  ggplot(lmreclosplot, aes(y=exp(emmean)-1, x=rec_los_av_income)) + geom_line() + 
    geom_point(data = LHS_weights, aes(x = rec_los_av_income, y = d_dk))+
    geom_ribbon(aes(ymin=exp(emmean-SE)-1, ymax=exp(emmean+SE)-1),col=NA, alpha=.3) + theme_bw()
  
  
  
  glmreclosplot = data.frame(emmeans(glm_reclos_weighted, ~rec_los_av_income+rec_los_density+rec_los_sex_ratio+rec_los_life_expct,
                                  at=list(rec_los_av_income=seq(0, 500000, by = 5000))))
  
  ggplot(glmreclosplot, aes(y=exp(emmean), x=rec_los_av_income)) + geom_line() + 
    geom_point(data = LHS_weights, aes(x = rec_los_av_income, y = d_dk))+
    geom_ribbon(aes(ymin=exp(emmean-SE), ymax=exp(emmean+SE)),col=NA, alpha=.3) + theme_bw()
  