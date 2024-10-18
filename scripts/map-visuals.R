## creating maps in R using jacobs life history data
## 
## Luke Watson
## 10/17/24


# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) # read xlsx files
library(usmap) # plotting us map
library(reshape2) # string split into cols

# load data ---------------------------------------------------------------

life_history_data_long <- read_xlsx("data/Final Project Data.xlsx")

# take a look at variables with colnames, str, and summary
colnames(life_history_data_long)

# mapping locations -------------------------------------------------------

# need to put data into map appropriate format state abbrev and county
locations_all <- life_history_data_long |> 
  select(starts_with("R") | starts_with("Location"))

locations_all <- cbind(
  locations_all, 
  colsplit(locations_all$`R1 Location`, ", ", names = c("r1_county", "r1_state"))
)
locations_all <- cbind(
  locations_all,
  colsplit(locations_all$`Location 2`, ", ", names = c("r2_county", "r2_state"))
)
locations_all <- cbind(
  locations_all,
  colsplit(locations_all$`R3 Location`, ", ", names = c("r3_county", "r3_state"))
)
locations_all <- cbind(
  locations_all,
  colsplit(locations_all$`R4 Location`, ", ", names = c("r4_county", "r4_state"))
)
locations_all <- cbind(
  locations_all,
  colsplit(locations_all$`R5 Location`, ", ", names = c("r5_county", "r5_state"))
)

# join state identifiers to this location all dataframe
locations_all <- left_join(locations_all, statepop, by = join_by("r1_state" == "full"))

# change names
locations_all <- locations_all |> 
  rename(
    r1_state_abb = abbr, 
    r1_state_fips = fips
  )

# repeat for r2-5 states
locations_all <- left_join(locations_all, statepop, by = join_by("r2_state" == "full"))
locations_all <- locations_all |> 
  rename(
    r2_state_abb = abbr, 
    r2_state_fips = fips
  )
locations_all <- left_join(locations_all, statepop, by = join_by("r3_state" == "full"))
locations_all <- locations_all |> 
  rename(
    r3_state_abb = abbr, 
    r3_state_fips = fips
  )
locations_all <- left_join(locations_all, statepop, by = join_by("r4_state" == "full"))
locations_all <- locations_all |> 
  rename(
    r4_state_abb = abbr, 
    r4_state_fips = fips
  )
locations_all <- left_join(locations_all, statepop, by = join_by("r5_state" == "full"))
locations_all <- locations_all |> 
  rename(
    r5_state_abb = abbr, 
    r5_state_fips = fips
  )

# now can integrate county level identifiers
locations_all <- left_join(locations_all, countypop, by = join_by("r1_state_abb" == "abbr", "r1_county" == "county"))
locations_all <- locations_all |> 
  rename(
    r1_county_fips = fips
  )
locations_all <- left_join(locations_all, countypop, by = join_by("r2_state_abb" == "abbr", "r2_county" == "county"))
locations_all <- locations_all |> 
  rename(
    r2_county_fips = fips
  )
locations_all <- left_join(locations_all, countypop, by = join_by("r3_state_abb" == "abbr", "r3_county" == "county"))
locations_all <- locations_all |> 
  rename(
    r3_county_fips = fips
  )
locations_all <- left_join(locations_all, countypop, by = join_by("r4_state_abb" == "abbr", "r4_county" == "county"))
locations_all <- locations_all |> 
  rename(
    r4_county_fips = fips
  )
locations_all <- left_join(locations_all, countypop, by = join_by("r5_state_abb" == "abbr", "r5_county" == "county"))
locations_all <- locations_all |> 
  rename(
    r5_county_fips = fips
  )

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
