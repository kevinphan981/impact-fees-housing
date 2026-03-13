library(tidyverse)
library(haven) # for stata dta files
library(stringr)
library(gt)
library(gtsummary)

wharton_2018 <- read_dta("clean-data/WRLURI_01_15_2020.dta")
wharton_2006 <- read_dta("clean-data/WRLURI_01_24_2008.dta")

# adding county fips to wharton data (aggregating using its weights of the full sample)
wharton_2018 <- wharton_2018 %>% 
  mutate(
    county_fips_3 = str_pad(countycode18, width = 3, pad = "0"),
    county_geoid  = paste0(statecode_str, county_fips_3),
    GEOID = substr(GEOID, 1, 7)
  )
wharton_2018_agg <- wharton_2018 %>%
  # select(!c("communityname18", "GEOID", "fipsplacecode18", "statecode", "state_code_str")) %>%
  group_by(county_geoid) %>%
  summarise(q9c18_county = max(q9c18)
            ) #max for exposure to impact fees

# still need to do the other ones...

elections <- read_csv("raw-data/ledb_candidatelevel.csv")
## cleaning elections data ##

unique_offices <- elections %>%
  distinct(office_consolidated)
# I choose to only filter by the following offices
# County Executive
# Mayor
# County Legislature
# City Council

# combined with the fact of them being winners, I take the average prob democrat
# i use the year prior for the elections to take place.

san_benardino <- elections %>%
  filter(fips == "06071", year == "2016")

elections_agg <- elections %>%
  filter(
    #year %in% (2014:2017), # i exclude 2005 for now.
    winner == "win",
    office_consolidated %in% c("County Executive", "Mayor", "County Legislature", "City Council"),
    nchar(fips) == 5
  ) %>%
  group_by(fips) %>% #we want a composite list of all four years prior to 2018.
  summarize(avg_prob_democrat = mean(prob_democrat, na.rm = T))

# prob democrat, republican, white did not work... 

# intention for instrument is to select the winners and the average prob of democrat

View(wharton) 

#q9c18 is a binary var for impact fees
#q1018 is a binary var for zoning, which has a positive corr with impact fees
#q16a118 is a num var for length of permit timing for single family homes, negative corr
#q8a18 is binary var for if there is a limit on single family housing

basic <- lm(q1018 ~ q9c18 + q9a18 + q9b18, data = wharton) 
summary(basic)

#+ q9a18 + q9b18
basic_permittime <- lm(q16a118 ~ q9c18, data = wharton) 
summary(basic_permittime)

#+ q9a18 + q9b18
basic_permit_limit <- lm(q8c18 ~ q9c18, data = wharton) 
summary(basic_permit_limit)


# descriptive stats at the jurisdiction level:

#requires sourcing the other tidycensus file if not done already
# source("raw-data/tidycensus.R")

juris_comp <- left_join(wharton_2018, place_housing, by = c("GEOID" = "GEOID"))
summary(lm(WRLURI18 ~ B01003_001E + B25001_001E + B25017_001E, data = juris_comp))

# basic housing prices and WRLURI composite score
ggplot(data = juris_comp, aes(x = WRLURI18, y = log(B25077_001E)))+
  geom_point(alpha = .5, color = "grey") + 
  geom_smooth(method = "lm", color = "red") +
  labs(y = "Log Median Home Values", x = "Wharton Index", 
       title = "Median Jurisdiction Home Values and Wharton Index 2018") +
  scale_x_continuous(limits = c(-3,5), breaks = seq(-3,5,1)) +
  theme_minimal(base_family = "palatino",
           paper = "white")


# housing units and wharton index
summary(lm(log(B25001_001E) ~ WRLURI18, data = juris_comp)) #alone index
summary(lm(log(B25001_001E) ~ WRLURI18 + B23025_003E + S1501_C02_015E + B19013_001E, data = juris_comp)) #alone index

ggplot(data = juris_comp, aes(x = WRLURI18, y = log(B25001_001E)))+
  geom_point(alpha = .5, color = "grey") + 
  geom_smooth(method = "lm", color = "red") +
  labs(y = "Log Housing Units", x = "Wharton Index", 
       title = "Log Housing Units and Wharton Index 2018") +
  scale_x_continuous(limits = c(-3,5), breaks = seq(-3,5,1)) +
  theme_minimal(base_family = "palatino")

# compare and contrast between home values in jurisdictions with impact fees and those without

juris_comp %>%
  filter(q9c18 == 2) %>%
  group_by(state) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  ggplot(data = ., aes(y = reorder(state, n), x = n)) +
  geom_bar(alpha = .75, color = "blue", stat = "identity") +
  labs(x = "Count", y = "States", title = "Communities with Impact Fees by State", caption = "Note: Hawaii was not included in the survey") +
  theme_minimal(base_family = "palatino")

# tables (most important part) must add_p() requires more recent dplyr

juris_comp %>%
  mutate(q9c18 = ifelse(q9c18 == 2, "No", "Yes")) %>%
  tbl_summary(
    ., 
    include = c(B25001_001E, B25077_001E, 
                B19013_001E, S1501_C02_015E,
                B23025_003E, B11007_001E, B01003_001E
                ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    by = q9c18,
    missing = "no",
    label = list(
        B25001_001E = "Housing Units", B25077_001E = "Median Home Value",
        B19013_001E = "Median Household Income", S1501_C02_015E = "Population with Bachelors or Higher (%)",
        B23025_003E = "Unemployment (Levels)", B11007_001E = "Households with Person 65+",
        B01003_001E = "Total Population"
                 )
)|> modify_header(label = "**Variable (Average)**") |>
    add_p() |> as_gt() %>% tab_header(title = "Differences in Communities with Impact Fees")

# instrument test
# 1. join data 

iv_test <- left_join(wharton_2018_agg, elections_agg, by = c("county_geoid" = "fips")) %>% na.omit()
iv_test <- iv_test %>% mutate(q9c18_county = as.character(q9c18_county))
summary(lm(q9c18_county ~ avg_prob_democrat, data = iv_test))

#exhibit 
ggplot(data = iv_test, aes(x = q9c18_county, y = avg_prob_democrat)) +
  geom_boxplot(median.color = "red", box.color = "darkgrey") +
  labs(x = "Exposure to Impact Fees", 
       y = "Average Probability of Being Democrat",
       title = "Impact Fees and Party Affiliation of Officials",
       caption = "Election data from 2014-2017 prior to latest WRLURI") +
  theme_minimal(base_family = "palatino")



