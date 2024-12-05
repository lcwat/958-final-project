## creating maps in R using jacobs life history data
## 
## Luke Watson
## 10/17/24


# load libraries ----------------------------------------------------------

library(tidyverse)
library(haven) # read spss sav files
library(readxl) # read xlsx
#library(usmap) # plotting us map
library(haven) # read sav
library(tidycensus) # tidy us census data
library(tigris) # county area for density
library(sf) # plotting maps
library(crsuggest) # suggest best cartographic set for dataset

# load data ---------------------------------------------------------------

# life_history_data_long <- read_sav("data/Final Project Data_long.sav")
life_history_data_long <- read_csv("data/life-hist-data-long.csv")
life_history_data_long <- read_xlsx("data/Final Project Data_long.xlsx")

# take a look at variables with colnames, str, and summary
colnames(life_history_data_long)


# census stuff ------------------------------------------------------------

# set census access key, only have to do once if install is set to true
census_api_key(
  "3d2b4132118c1c1c8fa942ecb3aa3680976bcada", 
  install = TRUE, overwrite = TRUE
)

readRenviron("~/.Renviron")

# check to see if it is in there
Sys.getenv("CENSUS_API_KEY")

# view variables (there are thousands, not super helpful, need to learn how to
# search this)
var_table <- load_variables(
  year = 2010, "sf1"
)

pop_county <- get_decennial(
  geography = "county",
  variables = "P001001", 
  year = 2010
)

pop_county <- pop_county |> 
  select(
    geo_id = GEOID, 
    county_state = NAME, 
    pop = value
  )


# merge w county sf file for plotting -----------------------------------------

# get sf data for plotting
area_county <- counties(cb = TRUE)

# rearrange geo to keep aleutian islands from expanding plot too much
# best for national mapping like I plan to do
area_county <- shift_geometry(area_county)

# get area
area_county <- area_county |> 
  dplyr::select(
    statefips = STATEFP,
    countyfips = COUNTYFP,
    county = NAMELSAD,
    state = STATE_NAME
  )

area_county <- area_county |> 
  mutate(
    geo_id = paste(statefips, countyfips, sep = ""), 
    location = paste(county, state, sep = ", ")
  )

head(area_county)

# remove NA locations
plot_me <- life_history_data_long |> 
  filter(!is.na(location))

# can merge w data for plotting with sf
plot_counties <- area_county |> 
  left_join(plot_me, by = join_by("location" == "location"))

# keep only contig us
plot_counties <- plot_counties |> 
  filter(statefips <= 56)

#

# what was cleaned --------------------------------------------------------

scan <- life_history_data_long |> 
  filter(is.na(geo_id)) |> 
  filter(is.na(Location) & !is.na(SexRatio))

scan <- scan[, 1:23]

# save file to debug
write_csv(scan, "data/missing-location-but-data-present.csv")

# appears to be some locations that are not present but have data, while some
# others appear to not have merged with the df either to different names or 
# missing pieces
myc <- counties(cb = F)

myc |> 
  filter(str_detect(NAMELSAD, "San Diego"))

mys <- states()

mys |> 
  filter(STATEFP == "48")
# Conn is fp 09

myc |> 
  filter(STATEFP == "48")
# somewhere along the way, the counties became planning regions, 
# fairfield county is a part of the Greater Bridgeport Planning Region now
# "Fairfield County" == "Greater Bridgeport Planning Region, Connecticut"

life_history_data_long |> 
  filter(str_detect(Location, "San Diego")) |> 
  select(Location)

# also Saint should not be spelled out, St. instead
# "Saint Clair County, Missouri" == "St. Clair County, Missouri"
# "Saint Clair County, Illinois" == "St. Clair County, Illinois"
# "Saint Charles County, Missouri" == "St. Charles County, Missouri"
# "El Paso County, Kansas" == "El Paso County, Texas"
# "Sedwick County, Kansas" == "Sedgwick County, Kansas"
# "Rockwall, Texas" == "Rockwall County, Texas"
# "Bethel Census Area" == "Bethel Census Area, Alaska"
# "Southeast Fairbanks Census Area, Kansas" == "Southeast Fairbanks Census Area, Alaska"
# "Tom Green County, Kansas" == "Tom Green County, Texas"
# "San Diego County, Kansas" == "San Diego County, California"

life_history_data_long <- life_history_data_long |> 
  rename(Location = location)

# use case when to replace misspellings
life_history_data_long <- life_history_data_long |> 
  mutate(
    location = case_when(
      Location == "Saint Clair County, Missouri" ~ "St. Clair County, Missouri",
      Location == "Saint Clair County, Illinois" ~ "St. Clair County, Illinois",
      Location == "Saint Charles County, Missouri" ~ "St. Charles County, Missouri",
      Location == "El Paso County, Kansas" ~ "El Paso County, Texas",
      Location == "Sedwick County, Kansas" ~ "Sedgwick County, Kansas",
      Location == "Rockwall, Texas" ~ "Rockwall County, Texas",
      Location == "Bethel Census Area" ~ "Bethel Census Area, Alaska",
      Location == "Southeast Fairbanks Census Area, Kansas" ~ "Southeast Fairbanks Census Area, Alaska",
      Location == "Tom Green County, Kansas" ~ "Tom Green County, Texas",
      Location == "San Diego County, Kansas" ~ "San Diego County, California",
      Location == "Fairfield County, Connecticut" ~ "Greater Bridgeport Planning Region, Connecticut",
      .default = Location
    )
  )

# save fixes
write_csv(life_history_data_long, "data/life-hist-data-long.csv")


# pop density -------------------------------------------------------------

pop_dens <- left_join(areas, pop_county)

pop_dens <- pop_dens |> 
  mutate(
    # pop per square mile
    density = pop / land_area * 1000 / .3861
  )

pop_dens |> 
  arrange(desc(density))

# find relevant vars through filter
sexvars <- var_table |> 
  filter(str_detect(concept, "SEX"))

# total female = P012026
pop_female <- get_decennial(
  geography = "county", 
  variables = "P012026", 
  year = 2010
)

# sex ratio
pop_female <- pop_female |> 
  select(
    geo_id = GEOID,
    pop_female = value
  )

pop_county <- left_join(pop_county, pop_female)

pop_county <- pop_county |> 
  mutate(
    sex_ratio = pop_female / pop
  )

summary(pop_county$sex_ratio) # looks good

# want variables sex ratio, life expectancy, pop density, income by county

# mapping locations -------------------------------------------------------

# us map code
# locations_to_include <- locations_all$r1_state_abb
# 
# plot_usmap(regions = "counties", data = locations_all, include = locations_to_include, values = "RiskTaking", color = "black") + 
#   scale_fill_continuous(
#     low = "green4", high = "green", 
#     name = "Risk Taking"
#   ) + 
#   theme(legend.position = "right")
# 
# plot_usmap(data = countypov, regions = "counties", values = "pct_pov_2021", color = "blue") +
#   scale_fill_continuous(
#     low = "blue4", high = "orange", 
#     name = "poverty percentage"
#   )

# plot sf
states <- states()

states <- states |> 
  filter(STATEFP <= 56)

# rearrange geo to keep aleutian islands from expanding plot too much
states <- shift_geometry(states)

# suggest crs
crsuggest::suggest_crs(plot_counties)

# same as already using, NAD83

# count number of obs from counties, some overrepresented in convenience sample data
sum_locations <- plot_counties |> 
  group_by(location) |> 
  summarise(
    total = sum(!is.na(DDk))
  )

# turn zero to na
sum_locations <- sum_locations |> 
  mutate(
    new_total = if_else(total == 0, NA, total)
  )

# plot count with sf
sum_locations |> 
  ggplot() +
  
  geom_sf(
    aes(fill = new_total), 
    color = NA
  ) + 
  
  geom_sf(
    data = states, 
    fill = NA, color = "grey95"
  ) +
  
  # fill the counties with data
  scale_fill_viridis_c(
    "Number Reported",
    option = "magma", na.value = "black",
    begin = .5, end = 1
  ) +
  
  theme_void()

# trying to make function to streamline map creation, but not working yet
plot_ave_county_data <- function(
    sf_df, states_sf_df, variable, variable_name = "My Variable",
    scale_fill_option = "magma",
    scale_begin = .5, scale_end = 1
  ) {
  # group by county and calc. ave. for that variable to represent whole county
  to_plot <- sf_df |> 
    group_by(location) |> 
    summarise(
      ave_var_over_county = mean(variable, na.rm = T)
    )
  
  # now plot it
  to_plot |> 
    ggplot() +
    
    geom_sf(
      aes(fill = ave_var_over_county), 
      color = NA
    ) + 
    
    geom_sf(
      data = states_sf_df, 
      fill = NA, color = "grey95"
    ) +
    
    # fill the counties with data
    scale_fill_viridis_c(
      variable_name,
      option = scale_fill_option, na.value = "black",
      begin = scale_begin, end = scale_end
    ) +
    
    theme_void()
}

toplot <- plot_counties |> 
  group_by(location) |> 
  summarize(
    mean_ddk = mean(DDk, na.rm = T)
  )

plot_ave_county_data(
  plot_counties, states, 
  variable = DDk, variable_name = "Delay Discounting Score",
  scale_fill_option = "magma"
)

ggsave(
  "plots/subj-by-county.png", device = "png",
  width = 12, height = 10, units = "in"
)
  
