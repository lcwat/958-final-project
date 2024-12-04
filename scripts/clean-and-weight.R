## WEIGHTING
## this file will clean and organize the location data to turn each variable
## into aggregated predictors based on recency or primacy
## 
## Luke Watson 
## 12/3/24



# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)


# load data ---------------------------------------------------------------

wide_lhs_data <- read_xlsx("data/Final Project Data_wide.xlsx")

# clean up names
names(wide_lhs_data) <- make_clean_names(names(wide_lhs_data))

# change x into r
names(wide_lhs_data) <- str_replace(names(wide_lhs_data), "x", "r")

# clean


# workshopping weighting --------------------------------------------------

# added two columns to indicate how long they stayed in that location in years
# and when they lived there
# length and move

# SET WEIGHTS HERE #
positions <- seq(1, 5, 1)

# decreasing linearly from 1st position when applied to difference
# 0.6666667 0.7333333 0.8000000 0.8666667 0.9333333
primacy_weights <- 1 - ((max(positions) - positions + 1) / sum(positions))
# or increasing linearly from 1st position
# 0.9333333 0.8666667 0.8000000 0.7333333 0.6666667
recency_weights <- 1 - (positions / sum(positions))

# could also do exponential decay
# 1.00000000 0.36787944 0.13533528 0.04978707 0.01831564
primacy_weights <- 1 / (exp((max(positions) - positions + 1) - 1))
recency_weights <- 1 / (exp(positions - 1))

####################

# pass in vector to these functions to do the weighting
primacy_weighting <- function(vec, transformation = "none") {
  # check to see if all NA
  if(length(vec[is.na(vec)]) == 5) {
    new_vec <- c(NA, NA, NA, NA, NA)
  } else {
    # multiply diff between vec by first value, pull more recent values towards 
    # first
    
    ## apply transformations here if needed
    if(transformation == "none") {
      new_vec <- vec - ((vec - vec[[1]]) * primacy_weights)
    } else if(transformation == "log") {
      # log transform
      vec <- log(vec)
      
      new_vec <- vec - ((vec - vec[[1]]) * primacy_weights)
    } else if(transformation == "root") {
      # square root
      vec <- sqrt(vec)
      
      new_vec <- vec - ((vec - vec[[1]]) * primacy_weights)
    } else if(transformation == "square") {
      # squared
      vec <- vec^2
      
      new_vec <- vec - ((vec - vec[[1]]) * primacy_weights)
    }
  }
  
  return(new_vec)
}

recency_weighting <- function(vec, transformation = "none") {
  # check to see if all NA
  if(length(vec[is.na(vec)]) == 5) {
    new_vec <- c(NA, NA, NA, NA, NA)
  } else {
    # multiply diff between vec by last value, pull more early values towards 
    # most recent
    
    ## apply transformations here if needed
    if(transformation == "none") {
      new_vec <- vec - ((vec - vec[[length(vec[!is.na(vec)])]]) * recency_weights)
    } else if(transformation == "log") {
      # log transform
      vec <- log(vec)
      
      new_vec <- vec - ((vec - vec[[length(vec[!is.na(vec)])]]) * recency_weights)
    } else if(transformation == "root") {
      # square root
      vec <- sqrt(vec)
      
      new_vec <- vec - ((vec - vec[[length(vec[!is.na(vec)])]]) * recency_weights)
    } else if(transformation == "square") {
      # squared
      vec <- vec^2
      
      new_vec <- vec - ((vec - vec[[length(vec[!is.na(vec)])]]) * recency_weights)
    }
  }
  
  return(new_vec)
}

# test
incomes <- c(60000, 70000, 150000, NA, NA)

recency_weighting(incomes, "root")

mean(primacy_weighting(incomes, "log"), na.rm = T)

# weight the variables
weighted_df <- wide_lhs_data |> 
  group_by(response_id) |> 
  summarize(
    prim_av_income = mean(
      primacy_weighting(c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av)),
      na.rm = T
    ), 
    rec_av_income = mean(
      recency_weighting(c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av)), 
      na.rm = T
    ), 
    mean_income = mean(
      c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
      na.rm = T
    ), 
    prim_density = mean(
      primacy_weighting(c(r1dens, r2dens, r3dens, r4dens, r5dens)), 
      na.rm = T
    ), 
    rec_density = mean(
      recency_weighting(c(r1dens, r2dens, r3dens, r4dens, r5dens)), 
      na.rm = T
    ), 
    mean_density = mean(
      c(r1dens, r2dens, r3dens, r4dens, r5dens), 
      na.rm = T
    ), 
    prim_sex_ratio = mean(
      primacy_weighting(c(r1sr, r2sr, r3sr, r4sr, r5sr)), 
      na.rm = T
    ),
    rec_sex_ratio = mean(
      recency_weighting(c(r1sr, r2sr, r3sr, r4sr, r5sr)), 
      na.rm = T
    ), 
    mean_sex_ratio = mean(
      c(r1sr, r2sr, r3sr, r4sr, r5sr), 
      na.rm = T
    ), 
    prim_life_expct = mean(
      primacy_weighting(c(r1le, r2le, r3le, r4le, r5le)), 
      na.rm = T
    ), 
    rec_life_expct = mean(
      recency_weighting(c(r1sr, r2sr, r3sr, r4sr, r5sr)), 
      na.rm = T
    ),
    mean_life_expct = mean(
      c(r1sr, r2sr, r3sr, r4sr, r5sr), 
      na.rm = T
    )
  )

# looks to be working! can join it in with the original dataframe
weighted_lhs_data <- left_join(wide_lhs_data, weighted_df)

# simplify data file
linear_weighted_lhs_agg_data <- weighted_lhs_data |> 
  dplyr::select(1:15, 61:72)

# file to use, should think about transformations before or after weighting, 
# I'm leaning towards before
# log <- income, density
# sqrt <- sex ratio
# square <- life expct