library(tidyverse)
library(fredr)


Sys.getenv("FRED_API_KEY")

fredr_series_search("unemployment", tag_names = "county")

