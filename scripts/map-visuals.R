## creating maps in R using jacobs life history data
## 
## Luke Watson
## 10/17/24


# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) # read xlsx files
library(usmap) # plotting us map
library(haven) # read sav
library(tidycensus) # tidy us census data
library(tigris) # county area for density
library(sf) # plotting maps

# load data ---------------------------------------------------------------

life_history_data_long <- read_sav("data/Final Project Data_long.sav")

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


# -------------------------------------------------------------------------

# get sf data for plotting
area_county <- counties(cb = TRUE)

# get area
area_county <- area_county |> 
  select(
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

# can merge w data for plotting with sf
life_history_data_long <- life_history_data_long |> 
  left_join(area_county, by = join_by("Location" == "location"))



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
plot_usmap(regions = "counties", data = locations_all, include = locations_to_include, values = "RiskTaking", color = "black") + 
  scale_fill_continuous(
    low = "green4", high = "green", 
    name = "Risk Taking"
  ) + 
  theme(legend.position = "right")

plot_usmap(data = countypov, regions = "counties", values = "pct_pov_2021", color = "blue") +
  scale_fill_continuous(
    low = "blue4", high = "orange", 
    name = "poverty percentage"
  )

states <- states()

states <- states |> 
  filter(STATEFP <= 56)

# plot with sf
states |> 
  ggplot() +
  
  geom_sf() +
  
  geom_sf(
    data = life_history_data_long, 
    aes(geometry = geometry, fill = PopulationDensity)
  ) + 
  
  theme_void()
  
  
