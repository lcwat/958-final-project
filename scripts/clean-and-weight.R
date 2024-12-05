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

## -----------------------------------------------------------------------------
##      weighting
## -----------------------------------------------------------------------------

# function for setting weights and can specify "option" for weighting
# either linear decreasing weight or exponential for larger dropoff
# set "weighting" to either primacy or recency for whichever is being used

set_weights <- function(number_pos, weighting, option = "linear") {
  # set the number of positions from length of sequence
  if(number_pos)
  
  positions <- seq(1, number_pos, 1)
  
  # if there is only one location, don't bother
  if(number_pos == 1) {
    # just use these vectors
    if(weighting == "primacy") {
      weight_vec <- c(1, 0, 0, 0, 0)
    } else if(weighting == "recency") {
      weight_vec <- c(0, 0, 0, 0, 1)
    } else {
      stop("Please specify proper weighting parameter: primacy or recency")
    }
  } else {
    # use the length to set vector of weights to be applied to data sequence of 
    # same length
    if(option == "linear") {
      if(weighting == "primacy") {
        # decreasing linearly from 1st position when applied to difference
        weights <- 1 - ((max(positions) - positions + 1) / sum(positions))
      } else if(weighting == "recency") {
        # or increasing linearly from 1st position
        weights <- 1 - (positions / sum(positions))
      } else {
        stop("Please specify proper weighting parameter: primacy or recency")
      }
    } else if(option == "exponential") {
      if(weighting == "primacy") {
        # decreasing exponentially from 1st position when applied to difference
        weights <- 1 / (exp((max(positions) - positions + 1) - 1))
      } else if(weighting == "recency") {
        # or increasing exponentially from 1st position
        weights <- 1 / (exp(positions - 1))
      } else {
        stop("Please specify proper weighting parameter: primacy or recency")
      }
    } else {
      stop("Please specify proper option: linear or exponential")
    }
  }
  
  
  
  # add zeros to match vector length of 5
  num_zero <- 5 - length(weights)
  vec_zero <- rep(0, num_zero)
  weight_vec <- c(weights, vec_zero)
  
  return(weight_vec)
}

# pass in vector to these functions to do the weighting
# can specify particular transformation to the variable: "none" for identity, 
# "log" for log(), "root" for sqrt(), "square" for x^2
# also can specify weighting function option: (default) "linear" for linear 
# weight decrease as a function of position and "exponential" for exponential 
# weight decrease

primacy_weighting <- function(vec, transformation = "none", option = "linear") {
  # check to see if all NA
  if(length(vec[is.na(vec)]) == 5) {
    new_vec <- c(NA, NA, NA, NA, NA)
  } else {
    # multiply diff between vec by first value, pull more recent values towards 
    # first
    
    # set weights according to length of sequence and option
    primacy_weights <- set_weights(
      length(!is.na(vec)), "primacy", option = option
    )
    
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