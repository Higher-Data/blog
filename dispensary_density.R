# Dispensaries per capita in the United States
# Created by Higher Data
# Analysis can be viewed on our blog: www.higher-data.com/our-research
# Data last updated: 10/31/19

# Load packages
library(readxl)
library(stats)
library(dplyr)
library(sf) 
library(spData) # us_states has pop for each state
library(cartogram)
library(tmap)

# Read in data from latest CannaRadar file (CanaRadar_all sheet)
cr_raw <- CannaRadar_October_19 <- read_excel("~/CannaRadar_October_19.xlsx", 
                                              sheet = "CannaRadar_all")
# Filter to dispesnaries, aggregate count by state
cr_disp <- cr_raw[cr_raw$license_type %in% c("Dispensary","Microbusiness", "Retail", 
                                             "Integrated","Comprehensive"),]
cr_disp$dummy <- 1
cr <- aggregate(dummy ~ region_name, cr_disp, sum)

# Grab geometries of US states from tigris package and convert to sf
states_sf <- tigris::states(cb = TRUE) %>% 
  st_as_sf() %>% 
  select(STUSPS, NAME, geometry) %>% 
  filter(!STUSPS %in% c("VI", "MP", "GU", "PR", "AS")) # filter out territories and Puerto Rico

# Join geo data with CannaRadar data
cr_sf <- states_sf %>% left_join(cr, by = c("NAME" = "region_name"))

# Alaska and Hawaii are removed for simplicity... we could rescale and add them back in if wanted
cr_sf2 <- cr_sf %>% filter(!NAME %in% c("Alaska", "Hawaii"))

# Grab Census population data from us_states in the spData package and join
states_pop <- us_states %>% 
  select(GEOID, NAME, total_pop_15) %>% 
  st_set_geometry(NULL)

cr_pop <- cr_sf2 %>%
  left_join(states_pop, by = "NAME")

# Normalize the population
cr_pop <- cr_pop %>%
  mutate(pop_norm = (dummy / total_pop_15 * 100000)) # try per 100,000 people?

# CREATING THE MAP
# Set the projection for mapping
cr_carto <- st_transform(cr_pop, crs = 2163)

cr_sp <- as(cr_carto, "Spatial")

# weigh size by state pop in non-contiguous cartogram
cr_carto <- nc_cartogram(cr_sp, weight = "total_pop_15", k = 0.5)

# Create sequential single hue color palette :: http://colorbrewer2.org/#type=sequential&scheme=Greens&n=5
greenpal <- c('#edf8e9','#bae4b3','#74c476','#31a354','#006d2c')

# Choose breaks based on what variation we're most interested in
# legend.reverse for HIGH values on TOP, slight sepia to offset white glare?
# fiddle with margins to fit legend and small title
# plot!
map <- tm_shape(cr_carto) +
  tm_borders("grey10") +
  tm_fill(title = "", "pop_norm",
          palette = greenpal,
          breaks = c(0,.5,1,5,25,50),
          textNA = "Zero",
          colorNA = "White",
          legend.reverse = T) +
  tm_layout(inner.margins = c(.04,.02, .08, .02),
            main.title = "Number of Dispensaries per 100,000 People",
            title = "State size by state population\n(Source: Higher Data CannaRadar)",
            title.position = c("center", "top"), title.size = 0.7,
            legend.text.size = 0.85, 
            sepia.intensity = 0.1)

map

save_tmap(map, "~/dispensary_density.png")

