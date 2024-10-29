## creating maps in R using jacobs life history data
## 
## Luke Watson
## 10/17/24


# load libraries ----------------------------------------------------------

library(tidyverse)
library(haven) # read spss sav files
library(usmap) # plotting us map
library(reshape2) # string split into cols

# load data ---------------------------------------------------------------

life_history_data_long <- read_sav("data/Final Project Data_long.sav")

# take a look at variables with colnames, str, and summary
colnames(life_history_data_long)

# mapping locations -------------------------------------------------------

# need to put data into map appropriate format state abbrev and county
life_history_data_long <- cbind(
  life_history_data_long, 
  colsplit(life_history_data_long$Location, ", ", names = c("county", "state"))
)

# join state identifiers to this location all dataframe
life_history_data_long <- left_join(life_history_data_long, statepop, by = join_by("state" == "full"))

# now can integrate county level identifiers
life_history_data_long <- left_join(life_history_data_long, countypov, by = join_by("abbr", "county"))

# now clean
# remove population cols leftover from join
locations_all <- locations_all |> 
  select(!starts_with("pop"))

# remove old location cols
locations_all <- locations_all |> 
  select(!contains("Location"))

# rename descriptives for county
locations_all <- locations_all |> 
  rename(
    r1_ave_income = R1in.av, 
    r2_ave_income = R2in.av, 
    r3_ave_income = R3in.av, 
    r4_ave_income = R4in.av,
    r5_ave_income = R5in.av, 
    r1_hh_income = R1HHin, 
    r2_hh_income = R2HHin,
    r3_hh_income = R3HHin,
    r4_hh_income = R4HHin,
    r5_hh_income = R5HHin,
    r1_roommates = R1roommates,
    r2_roommates = R2roommates,
    r3_roommates = R3roommates,
    r4_roommates = R4roommates,
    r5_roommates = R5roommates
  )

locations_all <- locations_all |> 
  relocate(
    ResponseId:RiskTaking,
    r1_county_fips, r1_county, r1_state_abb,
    r1_ave_income:R1LE,
    r2_county_fips, r2_county, r2_state_abb,
    r2_ave_income:R2LE,
    r3_county_fips, r3_county, r3_state_abb,
    r3_ave_income:R3LE,
    r4_county_fips, r4_county, r4_state_abb,
    r4_ave_income:R4LE,
    r5_county_fips, r5_county, r5_state_abb,
    r5_ave_income:R5LE
  )

# remove back end
locations_all <- locations_all |> 
  select(ResponseId:R5LE)

# turn r1fips into fips for plotting
locations_all <- locations_all |> 
  rename(fips = r1_county_fips)

locations_to_include <- locations_all$r1_state_abb

plot_usmap(regions = "counties", data = locations_all, include = locations_to_include, values = "RiskTaking", color = "black") + 
  scale_fill_continuous(
    low = "green4", high = "green", 
    name = "Risk Taking", label = label_number()
  ) + 
  theme(legend.position = "right")

plot_usmap(data = countypov, regions = "counties", values = "pct_pov_2021", color = "blue") +
  scale_fill_continuous(
    low = "blue4", high = "orange", 
    name = "poverty percentage", label = label_number()
  ) 
