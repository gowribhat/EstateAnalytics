# Loading household income by planning area
data <- read.csv("./data/raw/Household income by planning area.csv")

# Filtering out the "total" row in column "Number"
df <- data[data$Number != "Total", ]

# Loading planning area geojson
library(sf)
geo_data <- as.data.frame(st_read("./data/raw/district_and_planning_area.geojson"))

unmatched_areas <- setdiff(geo_data$planning_area, df$Number)
# We notice that a few values in geo_data do not appear in df, this is cause some of these areas are not residential, hence they will not be included in our data

library(dplyr)
merged_income_data <- left_join(df, geo_data, by = c("Number" = "planning_area"))

saveRDS(merged_income_data, "./data/clean/household_income_data.RDS")
