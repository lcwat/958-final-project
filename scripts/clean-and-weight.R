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

# pass in vector to this functions to do the weighting
# 
# args: 
# 1. vec = vector of length 5 (including NA)
# 2. length_vec = vector of length 
# 3. weighting = either "primacy" to pull aggregate towards earliest
# value, "recency" to pull aggregate towards most recent value, 
# "los" to pull average more towards values with longer lengths of stay, 
# "primacy_los" to combine both primacy and los weighting, and "recency_los"
# to apply both recency and los
# 4. transformation = specify particular transformation to the variable: 
# (default) "none" for identity, "log" for log(), "root" for sqrt(), 
# "square" for x^2
# 5. option: weighting type, (default) "linear" for linear weight decrease as a 
# function of position and "exponential" for exponential weight decrease

weighted_vector <- function(
    vec, length_vec, 
    weighting, transformation = "none", option = "linear"
  ) {
  # check to see if all NA
  if(sum(is.na(vec)) == 5) {
    new_vec <- c(NA, NA, NA, NA, NA)
  } else if(sum(!is.na(vec)) == 1) { 
    # no weighting or aggregating to be done
    ## apply transformations here if needed
    if(transformation == "none") {
      # nothing
    } else if(transformation == "log") {
      # log transform
      vec <- log(vec)
    } else if(transformation == "root") {
      # square root
      vec <- sqrt(vec)
    } else if(transformation == "square") {
      # squared
      vec <- vec^2
    }
    
    new_vec <- vec
  } else {
    ## apply transformations here if needed
    if(transformation == "none") {
      # nothing
    } else if(transformation == "log") {
      # log transform
      vec <- log(vec)
    } else if(transformation == "root") {
      # square root
      vec <- sqrt(vec)
    } else if(transformation == "square") {
      # squared
      vec <- vec^2
    }
    
    # find the actual length of the vector (avoids repeating code below)
    actual_length <- sum(!is.na(vec))
    
    # now depending on weighting chosen, alter formula for weights and how
    # it is applied to the difference from first or last value in sequence
    if(weighting == "primacy") {
      # set weights according to sequence and option
      primacy_weights <- set_weights(
        actual_length, "primacy", option = option
      )
      
      # apply weights to vector of values
      new_vec <- vec - ((vec - vec[[1]]) * primacy_weights)
    } else if(weighting == "recency") {
      # set weights
      recency_weights <- set_weights(
        actual_length, "recency", option = option
      )
      
      # apply weights to vector of values
      new_vec <- vec - ((vec - vec[actual_length]) * recency_weights)
    } else if(weighting == "los") {
      # set weights according to length of stay
      los_weights <- sapply(
        length_vec, function(x) x / sum(length_vec, na.rm = T)
      )
      
      # apply weights to vector of values (simple product)
      new_vec <- vec * los_weights
    } else if(weighting == "primacy_los") {
      # set weights according to length of stay and primacy
      los_weights <- sapply(
        length_vec, function(x) x / sum(length_vec, na.rm = T)
      )
      primacy_weights <- set_weights(
        actual_length, "primacy", option = option
      )
      
      # apply both to the vector of values (primacy first)
      new_vec <- (vec - ((vec - vec[[1]]) * primacy_weights)) * los_weights
    } else if(weighting == "recency_los") {
      # set weights according to length of stay and recency
      los_weights <- sapply(
        length_vec, function(x) x / sum(length_vec, na.rm = T)
      )
      recency_weights <- set_weights(
        actual_length, "recency", option = option
      )
      
      # apply both to the vector of values (recency first)
      new_vec <- 
        (vec - ((vec - vec[actual_length]) * recency_weights)) * los_weights
    } else {
      stop("Please specify proper weighting parameter: primacy or recency")
    }
  }
  
  return(new_vec)
}

# test
incomes <- c(60000, 70000, 150000, NA, NA)
length_of_stay <- c(10, 10, 2, NA, NA)

vect1 <- weighted_vector(incomes, length_of_stay, "recency")

log_vec <- weighted_vector(incomes, length_of_stay, "recency", "log")

log(vect1)

# slight difference

mean(weighted_vector(incomes, "primacy"), na.rm = T)


# weight the variables ----------------------------------------------------

weighted_df <- wide_lhs_data |> 
  group_by(response_id) |> 
  summarize(
    # average income vars
    mean_av_income = mean(
      c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
      na.rm = T
    ),
    prim_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy"
      ),
      na.rm = T
    ), 
    rec_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency"
      ),
      na.rm = T
    ),
    los_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "los"
      ),
      na.rm = T
    ), 
    prim_los_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy_los"
      ),
      na.rm = T
    ), 
    rec_los_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency_los"
      ),
      na.rm = T
    ),
    # density vars
    mean_density = mean(
      c(r1dens, r2dens, r3dens, r4dens, r5dens), 
      na.rm = T
    ),
    prim_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy"
      ),
      na.rm = T
    ), 
    rec_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency"
      ),
      na.rm = T
    ),
    los_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "los"
      ),
      na.rm = T
    ), 
    prim_los_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy_los"
      ),
      na.rm = T
    ), 
    rec_los_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency_los"
      ),
      na.rm = T
    ),
    # sex ratio vars
    mean_sex_ratio = mean(
      c(r1sr, r2sr, r3sr, r4sr, r5sr), 
      na.rm = T
    ),
    prim_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy"
      ),
      na.rm = T
    ), 
    rec_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency"
      ),
      na.rm = T
    ),
    los_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "los"
      ),
      na.rm = T
    ), 
    prim_los_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy_los"
      ),
      na.rm = T
    ), 
    rec_los_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency_los"
      ),
      na.rm = T
    ),
    # life expectancy vars
    mean_life_expct = mean(
      c(r1le, r2le, r3le, r4le, r5le), 
      na.rm = T
    ),
    prim_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy"
      ),
      na.rm = T
    ), 
    rec_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency"
      ),
      na.rm = T
    ),
    los_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "los"
      ),
      na.rm = T
    ), 
    prim_los_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy_los"
      ),
      na.rm = T
    ), 
    rec_los_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency_los"
      ),
      na.rm = T
    )
  )

# looks to be working! can join it in with the original dataframe
weighted_lhs_data <- left_join(wide_lhs_data, weighted_df)

# simplify data file
linear_weighted_lhs_agg_data <- weighted_lhs_data |> 
  dplyr::select(1:15, 61:90)

# no transformations applied prior, but have option to do so with extra argument
write_csv(linear_weighted_lhs_agg_data, "data/lin-weight-untransf-lhs-data.csv")


# transform and weight ----------------------------------------------------

# transformations
# log <- income, density
# sqrt <- sex ratio
# square <- life expct

# weight the variables, but transform first
transf_weighted_df <- wide_lhs_data |> 
  group_by(response_id) |> 
  summarize(
    # average income vars
    log_mean_av_income = mean(
      log(c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av)), 
      na.rm = T
    ),
    log_prim_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy", "log"
      ),
      na.rm = T
    ), 
    log_rec_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency", "log"
      ),
      na.rm = T
    ),
    log_los_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "los", "log"
      ),
      na.rm = T
    ), 
    log_prim_los_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy_los", "log"
      ),
      na.rm = T
    ), 
    log_rec_los_av_income = mean(
      weighted_vector(
        c(r1in_av, r2in_av, r3in_av, r4in_av, r5in_av), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency_los", "log"
      ),
      na.rm = T
    ),
    # density vars
    log_mean_density = mean(
      log(c(r1dens, r2dens, r3dens, r4dens, r5dens)), 
      na.rm = T
    ),
    log_prim_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy", "log"
      ),
      na.rm = T
    ), 
    log_rec_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency", "log"
      ),
      na.rm = T
    ),
    log_los_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "los", "log"
      ),
      na.rm = T
    ), 
    log_prim_los_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy_los", "log"
      ),
      na.rm = T
    ), 
    log_rec_los_density = mean(
      weighted_vector(
        c(r1dens, r2dens, r3dens, r4dens, r5dens), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency_los", "log"
      ),
      na.rm = T
    ),
    # sex ratio vars
    sqrt_mean_sex_ratio = mean(
      sqrt(c(r1sr, r2sr, r3sr, r4sr, r5sr)), 
      na.rm = T
    ),
    sqrt_prim_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy", "root"
      ),
      na.rm = T
    ), 
    sqrt_rec_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency", "root"
      ),
      na.rm = T
    ),
    sqrt_los_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "los", "root"
      ),
      na.rm = T
    ), 
    sqrt_prim_los_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy_los", "root"
      ),
      na.rm = T
    ), 
    sqrt_rec_los_sex_ratio = mean(
      weighted_vector(
        c(r1sr, r2sr, r3sr, r4sr, r5sr), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency_los", "root"
      ),
      na.rm = T
    ),
    # life expectancy vars
    sq_mean_life_expct = mean(
      (c(r1le, r2le, r3le, r4le, r5le))^2, 
      na.rm = T
    ),
    sq_prim_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy", "square"
      ),
      na.rm = T
    ), 
    sq_rec_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency", "square"
      ),
      na.rm = T
    ),
    sq_los_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "los", "square"
      ),
      na.rm = T
    ), 
    sq_prim_los_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "primacy_los", "square"
      ),
      na.rm = T
    ), 
    sq_rec_los_life_expct = mean(
      weighted_vector(
        c(r1le, r2le, r3le, r4le, r5le), 
        c(r1length, r2length, r3length, r4length, r5length), 
        "recency_los", "square"
      ),
      na.rm = T
    )
  )

# save it with transf applied
df <- left_join(wide_lhs_data, transf_weighted_df)

# simplify data file
df <- df |> 
  dplyr::select(1:15, 61:90)

write_csv(df, "data/transf-lin-weight-lhs-data.csv")

weighted_df |> 
  ggplot(aes(x = log(mean_av_income))) +
  geom_density()
