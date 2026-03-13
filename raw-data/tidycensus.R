
packages <- c("tidyverse", "tidyquant", "tidycensus", "stringr", "haven")

# Check which are missing and install them
missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages, dependencies = TRUE)

# Load all packages
lapply(packages, library, character.only = TRUE)

Sys.getenv("CENSUS_API_KEY")

# list of variables/tables to fetch
# suffix E is for estimate, suffix M is for margin of error.
# it may be better to get county for certain things...
variables_list <- c("B01003_001", "B25001_001", "B25017_001", 
                    "B25018_001", "B25034_001", "B25035_001", 
                    "B25041_001", "B25056_001", "B25058_001",
                    "B01001_001", "B09002_001", 
                    "B11007_001", "S1501_C02_015", "B19001_001",
                    "B23025_003", "B19013_001", "B01001_001", "B25077_001")

var <- load_variables(year = 2010, dataset = "acs5", cache = T)

test <- get_acs(
  geography = "tract",
  variables = variables_list,
  survey = "acs5",
  state = 15,
  year = 2024,
  output = "wide"
) 


# replication 1: median home values using the 2020 5-year ACS.
# this is generally right.

median_val_2020 <- get_acs(
  geography = "state",
  variables = "B25077_001",
  survey = "acs5",
  year = 2020,
  output = "wide"
) %>% arrange(B25077_001E)

ggplot(data = median_val_2020, aes(y = reorder(NAME, B25077_001E), x = B25077_001E)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  theme_bw() +
  xlab("Median Value (Nominal 2020)") + ylab("States")


# PLACE-based housing figures, using 2006-2010 ACS (use the latter year)
# we have to use the
#maybe I should change it to 2014-2018 rather than 2018-2022

focus = 2018 #subject to change

county_housing <- get_acs(
  geography = "county",
  variables = variables_list, #there is something extra that isn't working..
  year = focus,
  survey = "acs5",
  output = "wide"
)

place_housing <- get_acs(
  geography = "place",
  variables = variables_list, #there is something extra that isn't working..
  year = focus,
  survey = "acs5",
  output = "wide"
)


# the infamous property taxes variable
# B25103

taxes <- get_acs(
  geography = "place",
  variables = "B25103_001", #there is something extra that isn't working..
  year = 2022,
  survey = "acs5",
  output = "wide"
)

