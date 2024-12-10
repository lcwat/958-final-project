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
library(showtext) # font import and graphics output
library(readxl) # read xlsx files
library(boot) # cross validation and bootstrapping
library(glm2)
library(cv) # cross validation
library(lme4) # glm
library(MASS)
library(performance) # assumption checks
library(patchwork)
library(emmeans)
library(broom)
library(qpcR) # akaike weights

# load and view data ------------------------------------------------------

## original files, uncomment to load in 
# LHS_long <- read_xlsx("data/Final Project Data_long.xlsx")

our_seed <- 958958

set.seed(our_seed)

# adjusted predictors file (untransformed)
LHS_wide <- read_xlsx("data/Final Project Data_wide.xlsx")
LHS_weights <- read_csv("data/cleaned-lhs-weights.csv")
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
  ggplot(aes(x = prim_life_expct, y = log(d_dk))) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "none")

# check distrib of outcome or predictors
LHS_weights |> 
  ggplot(aes(x = sqrt(prim_density))) +
  geom_density()


# take a look at variables with colnames, str, and summary
  colnames(LHS_long)
  str(LHS_long) # variable types
  summary(LHS_long)
  

# theme set ---------------------------------------------------------------

# color palette
clrs <- NatParksPalettes::natparks.pals("Cuyahoga")
  
  # add font from google
  font_add_google("Ledger")
  font_add_google("Comfortaa")
  
  # ensure font is displayed in plot viewer
  showtext_auto()
  
  # ensure showtext renders to ggsave, will make text look huge in viewer!
  showtext_opts(dpi = 300)
  
our_theme <- function(option = "bw") {
  if(option == "bw") {
    theme_bw() + 
      theme(
        title = element_text(family = "Ledger", color = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        plot.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "grey80", color = NA), 
        legend.position = "none",
        panel.border = element_blank(),
        axis.text = element_text(family = "Comfortaa", color = "gray85"),
        axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "gray90"),
        axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "gray90"), 
        axis.ticks = element_line(linewidth = .35, linetype = "solid", color = "gray85")
      ) 
  } else if (option == "void") {
    theme_void() +
      theme(
        title = element_text(family = "Ledger", color = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "grey80", color = NA),
        legend.text = element_text(family = "Comfortaa", color = "gray90"),
        legend.background = element_rect(fill = "black")
      )
  }
}

# functions ---------------------------------------------------------------

# general function for bootstrapping regression parameters
# 1. pass in model type, either "lm" or "glm" (for Gamma(link = "log"))
# 2. pass in model formula y ~ x1 + x2
# boot will fill the other two to subset the data
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
      coef(glm2(
        formula,
        family = Gamma(link = "log"), # gamma error with log link
        data = data
      ))
    )
  } else {
    stop("Hey buddy, specify model type within quotes")
  }
}
  
# view parameter bootstrapped distributions with 95% CI
plot_param_dist <- function(boot_out, par_num, par_name) {
  # get ci
  ci <- boot.ci(boot_out, type = "bca", index = par_num)

  
  cis <- c(ci[[4]][4], ci[[4]][5])
  
  # calc density for bootstrapped weights
  dens <- density(boot_out$t[, par_num])
  
  plot_boot <- tibble(x = dens$x, y = dens$y) |> 
    mutate(
      variable = case_when(
        (x < cis[1] | x > cis[2]) ~ "On",
        (x >= cis[1] & x <= cis[2]) ~ "Off",
        TRUE ~ NA_character_
      )
    )
  
  p <- plot_boot |> 
    ggplot(aes(x, y)) +
    
    # define areas
    geom_area(
      data = filter(plot_boot, variable == 'On'), fill = clrs[2]
    ) +
    geom_area(
      data = filter(plot_boot, variable == 'Off'), fill = clrs[3]
    ) +
    
    # add line for zero
    geom_vline(
      aes(xintercept = 0), linetype = "dotted", linewidth = 1.25,
      color = clrs[2]
    ) +
    
    labs(title = par_name) +
    our_theme()
  
  return(p)
}
  
# Linear Analysis ---------------------------------------------------------
  #unweighted, log transform outcome and two skewed predictors
  lm_agg_unweighted <- lm(
    d_dk ~ mean_av_income + mean_density + mean_sex_ratio + mean_life_expct, 
    data = LHS_weights
  )
  summary(lm_agg_unweighted)
  tidy(lm_agg_unweighted)
  #unweighted
  lm_agg_unweighted <- lm(d_dk ~ mean_av_income + mean_density + mean_sex_ratio + mean_life_expct, 
    data = LHS_weights)
      summary(lm_agg_unweighted)
  
  check_model(lm_agg_unweighted)
  
  # bootstrap regression weights
  unweighted_boot <- boot(
    LHS_weights, statistic = boot_param, R = 2500, modeltype = "lm",
    formula = d_dk ~ mean_av_income + mean_density + mean_sex_ratio + mean_life_expct
  )

  # view hist of bootstrapped values
  int <- plot_param_dist(unweighted_boot, 1, "Intercept")
  income <- plot_param_dist(unweighted_boot, 2, "Mean Income")
  density <- plot_param_dist(unweighted_boot, 3, "Density")
  sr <- plot_param_dist(unweighted_boot, 4, "Sex Ratio")
  le <- plot_param_dist(unweighted_boot, 5, "Life Expectancy")
  
  p <- (int + income + density) + sr + le + 
    plot_annotation(theme = theme(plot.background = element_rect(fill = "black", color = NA)))
  
  ggsave(
    plot = p, "plots/boot-lm-coef-unweighted.png", device = "png", 
    width = 10, height = 6, units = "in"
  )
  
  # boot seems to suggest effect of sex when orig model does not, wary of this
  # result given many assumptions are being violated like linearity, normality
  # of residuals, homogeneity of variance
  

# plot model results ------------------------------------------------------

  # plot me of sex ratio
  density_toplot_lm <- as_tibble(
    emmeans(
    lm_agg_unweighted, 
    ~ mean_av_income + mean_density + mean_sex_ratio + mean_life_expct, 
    at = list(mean_density = seq(0, 41000, 500))
    )
  )
  
  LHS_weights |> 
    ggplot() +
    
    # plot orig data
    geom_jitter(
      aes(x = mean_density, y = d_dk), alpha = .8, color = clrs[3]
    ) +
    
    # plot fitted line
    geom_line(
      data = density_toplot_lm,
      aes(x = mean_density, y = emmean), 
      color = clrs[2], linewidth = 1.25
    ) +
    # error ribbons
    geom_ribbon(
      data = density_toplot_lm, 
      aes(x = mean_density, ymin = lower.CL, ymax = upper.CL), 
      alpha = .3, fill = clrs[2], color = NA
    ) +
    # horizontal line 
    geom_hline(aes(yintercept = 0), alpha = .4, color = "grey90") +
    
    scale_y_continuous(limits = c(-.1, .3), n.breaks = 5) +
    scale_x_continuous(n.breaks = 8) +
    
    labs(
      x = "Mean Density", 
      y = "Delay Discounting Score"
    ) +
    
    our_theme()
  
  # save
  ggsave(
    "plots/lm-density-me.png", device = "png", 
    width = 8, height = 6, units = "in"
  )


# weighted linear analysis -----------------------------------------------------
  
  #time-weighting
  lm_los <- lm(
    d_dk ~ los_av_income + los_density + los_sex_ratio + los_life_expct, 
    data = LHS_weights
  )
  
  tidy(lm_los)
  
  # bootstrap regression weights
  los_boot <- boot(
    LHS_weights, statistic = boot_param, R = 2500, modeltype = "lm",
    formula = d_dk ~ los_av_income + los_density + los_sex_ratio + los_life_expct
  )
  
  # view hist of bootstrapped values
  int <- plot_param_dist(los_boot, 1, "Intercept")
  income <- plot_param_dist(los_boot, 2, "Mean Income")
  density <- plot_param_dist(los_boot, 3, "Density")
  sr <- plot_param_dist(los_boot, 4, "Sex Ratio")
  le <- plot_param_dist(los_boot, 5, "Life Expectancy")
  
  p <- (int + income + density) + sr + le + 
    plot_annotation(theme = theme(plot.background = element_rect(fill = "black", color = NA)))
  
  ggsave(
    plot = p, "plots/boot-lm-coef-los.png", device = "png", 
    width = 10, height = 6, units = "in"
  )

  # primacy weighting
  lm_prim_weighted <- lm(
    d_dk ~ prim_av_income + prim_density + prim_sex_ratio + prim_life_expct, 
    data = LHS_weights
  )
  
  tidy(lm_prim_weighted)
  
  prim_boot <- boot(
    LHS_weights, statistic = boot_param, R = 2500, modeltype = "lm",
    formula = d_dk ~ prim_av_income + prim_density + prim_sex_ratio + prim_life_expct
  )
  
  # view hist of bootstrapped values
  int <- plot_param_dist(prim_boot, 1, "Intercept")
  income <- plot_param_dist(prim_boot, 2, "Mean Income")
  density <- plot_param_dist(prim_boot, 3, "Density")
  sr <- plot_param_dist(prim_boot, 4, "Sex Ratio")
  le <- plot_param_dist(prim_boot, 5, "Life Expectancy")
  
  p <- (int + income + density) + sr + le + 
    plot_annotation(theme = theme(plot.background = element_rect(fill = "black", color = NA)))
  
  ggsave(
    plot = p, "plots/boot-lm-coef-prim.png", device = "png", 
    width = 10, height = 6, units = "in"
  )
  
  # again, like unweighted model, suggests an effect of sex ratio when analysis
  # likely a type I error from bootstrapping an inappropriate model
  

# plot me of sr -----------------------------------------------------------
  
  # plot me of sex ratio
  toplot <- as_tibble(
    emmeans(
      lm_prim_weighted, 
      ~ prim_av_income + prim_density + prim_sex_ratio + prim_life_expct,
      at = list(prim_sex_ratio = seq(20, 90, 1))
    )
  )
  var <- prim_sex_ratio
  name <- "Primacy Sex Ratio"
  
  LHS_weights |> 
    ggplot() +
    
    # plot orig data
    geom_jitter(
      aes(x = var, y = d_dk), alpha = .8, color = clrs[3]
    ) +
    
    # plot fitted line
    geom_line(
      data = toplot,
      aes(x = prim_sex_ratio, y = emmean), 
      color = clrs[2], linewidth = 1.25
    ) +
    # error ribbons
    geom_ribbon(
      data = toplot, 
      aes(x = prim_sex_ratio, ymin = lower.CL, ymax = upper.CL), 
      alpha = .3, fill = clrs[2], color = NA
    ) +
    # horizontal line 
    geom_hline(aes(yintercept = 0), alpha = .4, color = "grey90") +
    
    scale_y_continuous(limits = c(-.1, .3), n.breaks = 5) +
    scale_x_continuous(n.breaks = 8) +
    
    labs(
      x = name, 
      y = "Delay Discounting Score"
    ) +
    
    our_theme()
  
  # save
  ggsave(
    "plots/prim-lm-sr-me.png", device = "png", 
    width = 8, height = 6, units = "in"
  )

# -------------------------------------------------------------------------

  # recency weighting
  lm_rec_weighted <- lm(
    d_dk ~ rec_av_income + rec_density + rec_sex_ratio + rec_life_expct, 
    data = LHS_weights
  )
  
  tidy(lm_rec_weighted)
  # sex ratio is marginally significant, lets see what boot does
  
  rec_boot <- boot(
    LHS_weights, statistic = boot_param, R = 2500, modeltype = "lm",
    formula = d_dk ~ rec_av_income + rec_density + rec_sex_ratio + rec_life_expct
  )
  
  # view hist of bootstrapped values
  int <- plot_param_dist(rec_boot, 1, "Intercept")
  income <- plot_param_dist(rec_boot, 2, "Mean Income")
  density <- plot_param_dist(rec_boot, 3, "Density")
  sr <- plot_param_dist(rec_boot, 4, "Sex Ratio")
  le <- plot_param_dist(rec_boot, 5, "Life Expectancy")
  
  p <- (int + income + density) + sr + le + 
    plot_annotation(theme = theme(plot.background = element_rect(fill = "black", color = NA)))
  
  ggsave(
    plot = p, "plots/boot-lm-coef-rec.png", device = "png", 
    width = 10, height = 6, units = "in"
  )
  

# plot me of sr -----------------------------------------------------------

  # plot me of sex ratio
  toplot <- as_tibble(
    emmeans(
      lm_rec_weighted, 
      ~ rec_av_income + rec_density + rec_sex_ratio + rec_life_expct,
      at = list(rec_sex_ratio = seq(20, 90, 1))
    )
  )
  
  LHS_weights |> 
    ggplot() +
    
    # plot orig data
    geom_jitter(
      aes(x = rec_sex_ratio, y = d_dk), alpha = .8, color = clrs[3]
    ) +
    
    # plot fitted line
    geom_line(
      data = toplot,
      aes(x = rec_sex_ratio, y = emmean), 
      color = clrs[2], linewidth = 1.25
    ) +
    # error ribbons
    geom_ribbon(
      data = toplot, 
      aes(x = rec_sex_ratio, ymin = lower.CL, ymax = upper.CL), 
      alpha = .3, fill = clrs[2], color = NA
    ) +
    # horizontal line 
    geom_hline(aes(yintercept = 0), alpha = .4, color = "grey90") +
    
    scale_y_continuous(limits = c(-.1, .3), n.breaks = 5) +
    scale_x_continuous(n.breaks = 8) +
    
    labs(
      x = "Recency Sex Ratio", 
      y = "Delay Discounting Score"
    ) +
    
    our_theme()
  
  # save
  ggsave(
    "plots/rec-lm-sr-me.png", device = "png", 
    width = 8, height = 6, units = "in"
  )
  
  # does appear to be a stronger relationship, with the line dipping below 0, 
  # not just the error
  # maybe this represents a better variation in the more recent estimates of the
  # sr, not as bunched up around 50 (default type of guess)
  

# model comparison --------------------------------------------------------
aics <- AIC(lm_agg_unweighted, lm_los, lm_prim_weighted, lm_rec_weighted)
# warning b/c not all models have the same number of observations!

# code for aic weights
akaike.weights(aics[,2])

# $weights
# unweighted, los, prim, rec
# [1] 0.43091902 0.28327685 0.23596382 0.04984031
# 43% likely that unweighted model is best among candidates
  
cv(lm_agg_unweighted, seed = our_seed) # cross-validation criterion (mse) = 0.00543554 
cv(lm_los, seed = our_seed) # cross-validation criterion (mse) = 0.006279236
cv(lm_prim_weighted, seed = our_seed) # cross-validation criterion (mse) = 0.006513087
cv(lm_rec_weighted, seed = our_seed) # cross-validation criterion (mse) = 0.004661342

# seems to suggest lowest error from recency weighted model, AIC may not be trust-
# worthy, cv results could be more reliable due to missing data
# looked back at the data, there is an odd participant who listed three locations
# but only provide estimates for some categories like density for all three, but
# for life expectancy, only have 2, which seems to mess with the weighting algor as
# a weird edge case, but not sure why this would lead to different ans for each
# weighting

# Generalized Linear Analysis ---------------------------------------------
              
  #unweighted
  glm_agg_unweighted <- glm2(
    d_dk ~ mean_av_income + mean_density + mean_sex_ratio + mean_life_expct, 
    data = LHS_weights, 
    family = Gamma(link = "log")
  )

  tidy(glm_agg_unweighted)
  # sex ratio effect emerges! others seem to come closer
  
  # bootstrap regression weights, most samples I can get is 250 on desktop
  unweighted_glm_boot <- boot(
    LHS_weights, statistic = boot_param, R = 2500, modeltype = "glm",
    formula = d_dk ~ mean_av_income + mean_density + mean_sex_ratio + mean_life_expct
  )

  # view hist of bootstrapped values
  int <- plot_param_dist(unweighted_glm_boot, 1, "Intercept")
  income <- plot_param_dist(unweighted_glm_boot, 2, "Mean Income")
  density <- plot_param_dist(unweighted_glm_boot, 3, "Density")
  sr <- plot_param_dist(unweighted_glm_boot, 4, "Sex Ratio")
  le <- plot_param_dist(unweighted_glm_boot, 5, "Life Expectancy")
  
  p <- (int + income + density) + sr + le + 
    plot_annotation(theme = theme(plot.background = element_rect(fill = "black", color = NA)))
  
  # a little more conservative, says sex ratio interval includes 0
  
  ggsave(
    plot = p, "plots/boot-glm-coef-unweighted.png", device = "png", 
    width = 10, height = 6, units = "in"
  )

  #time-weighting
      glm_los <- glm(d_dk~tw_in+tw_dens+tw_sr+tw_le, data=LHS_weights, family=Gamma(link="log"))

  #primacy weighting
      glm_prim_weighted <- glm(d_dk~prim_av_income+prim_density+prim_sex_ratio+prim_life_expct, data=LHS_weights, family=Gamma(link="log"))
             
  #recency weighting
      glm_rec_weighted <- glm(d_dk~rec_av_income+rec_density+rec_sex_ratio+rec_life_expct, data=LHS_weights, family=Gamma(link="log"))
        

# Model Comparisons -------------------------------------------------------
  summary(lm_agg_unweighted)
  summary(lm_time_weighted)
  summary(lm_prim_weighted)
  summary(lm_rec_weighted)
  
  summary(glm_agg_unweighted)
  summary(glm_time_weighted)
  summary(glm_prim_weighted)
  summary(glm_rec_weighted)
  
  AIC(lm_agg_unweighted)
  AIC(lm_los)
  AIC(lm_prim_weighted)
  AIC(lm_rec_weighted)

  
  AIC(glm_agg_unweighted)
  AIC(glm_los)
  AIC(glm_prim_weighted)
  AIC(glm_rec_weighted)
  
  akaike(glm_prim_weighted, glm_rec_weighted)
  
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
  toplot = data.frame(emmeans(lm_rec_weighted, ~rec_av_income+rec_density+rec_sex_ratio+rec_life_expct,
      at=list(rec_sex_ratio=seq(20, 80, by = 5))))
  
  ggplot(toplot, aes(y=exp(emmean)-1, x=mean_sex_ratio)) + geom_line() + 
    geom_point(data = LHS_weights, aes(x = mean_sex_ratio, y = d_dk))+
    geom_ribbon(aes(ymin=exp(emmean-SE)-1, ymax=exp(emmean+SE)-1),col=NA, alpha=.3) + theme_bw()+
    xlab("Sex Ratio (% male)") +ylab("Delay Discounting") +ggtitle("Unweighted Linear Prediction Model")
  
  toplot2 = data.frame(emmeans(glm_rec_weighted, ~ rec_av_income + rec_density + rec_sex_ratio + rec_life_expct,
    at=list(rec_sex_ratio=seq(20, 80, by = 5))))
  
  ggplot(toplot2, aes(y=exp(emmean), x=rec_sex_ratio)) + geom_line(color=clrs[2]) +
    geom_point(data = LHS_weights, aes(x = rec_sex_ratio, y = d_dk), color=clrs[3])+
    geom_ribbon(aes(ymin=exp(emmean-SE), ymax=exp(emmean+SE)),fill=clrs[2], color=NA, alpha=.3) + our_theme()+
    xlab("Sex Ratio") +ylab("Delay Discounting") +ggtitle("Recency GLM")
  
ggsave(filename="glm_rec.png", device="png")


# tables -----------------------------------------------------------------

summary(lm_agg_unweighted)
summary(lm_time_weighted)
summary(lm_prim_weighted)
summary(lm_rec_weighted)

summary(glm_agg_unweighted)
summary(glm_time_weighted)
summary(glm_prim_weighted)
summary(glm_rec_weighted)

lm_agg_unweighted_t<-tidy(lm_agg_unweighted)
write.csv(lm_agg_unweighted_t, "lm_agg_unweighted.csv")

lm_los_t<-tidy(lm_los)
write.csv(lm_los_t, "lm_los.csv")

lm_prim_weighted_t<-tidy(lm_prim_weighted)
write.csv(lm_prim_weighted_t, "lm_prim_weighted.csv")

lm_rec_weighted_t<-tidy(lm_rec_weighted)
write.csv(lm_rec_weighted, "lm_rec_weighted.csv")

glm_agg_unweighted_t<-tidy(glm_agg_unweighted)
write.csv(glm_agg_unweighted_t, "glm_agg_unweighted.csv")

glm_los_t<-tidy(glm_los)
write.csv(glm_los_t, "glm_los.csv")

glm_prim_weighted_t<-tidy(glm_prim_weighted)
write.csv(glm_prim_weighted_t, "glm_prim_weighted.csv")

glm_rec_weighted_t<-tidy(glm_rec_weighted)
write.csv(glm_rec_weighted, "glm_rec_weighted.csv")
