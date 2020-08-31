# Introduction ------------------------------------------------------------

# This code gathers alternative datasets that could be useful to complement
# bank supervision. The current version only downloads the datasets.
#
# Author: Douglas Araujo, Basel Committee Secretariat, BIS (Basel, Switzerland)


# Load packages -----------------------------------------------------------

library(data.table)
library(tidyverse)
library(gganimate)

# Change in mobility in BCBS jurisdictions -------

GoogleMobilityDataset <- data.table(read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
                                      stringsAsFactors = FALSE))
BCBS_members_observers <- c("AE",
                            "AR",
                            "AU",
                            "BE",
                            "BR",
                            "CA",
                            "CH",
                            "CL",
                            "DE",
                            "ES",
                            "FR",
                            "HK",
                            "ID",
                            "IN",
                            "IT",
                            "JP",
                            "LU",
                            "NL",
                            "RU",
                            "SA",
                            "SE",
                            "SG",
                            "TR",
                            "US",
                            "ZA")
GoogleMobilityBCBS <- GoogleMobilityGoogleMobilityDataset[country_region_code %in% BCBS_members_observers & sub_region_1 == "",]
GoogleMobilityBCBS <- GoogleMobilityDataset %>% melt(variable.name = "variable",
                                          value.names = "mobility_change",
                                          id.vars = c("country_region_code", "date"),
                                          measure.vars = c("retail_and_recreation_percent_change_from_baseline",
                                                           "grocery_and_pharmacy_percent_change_from_baseline",
                                                           "parks_percent_change_from_baseline",
                                                           "transit_stations_percent_change_from_baseline",
                                                           "workplaces_percent_change_from_baseline",
                                                           "residential_percent_change_from_baseline"))
GoogleMobilityBCBS[, date := as.POSIXct(paste0(date, " 00:00:00"))]
GoogleMobilityBCBS[, month := month(date)]
BCBS_mobility <- GoogleMobilityBCBS %>% select(-month) %>%  filter(variable == "retail_and_recreation_percent_change_from_baseline") %>% 
  filter(complete.cases(.)) %>% 
  ggplot(aes(x = date, y = value,
             colour = country_region_code)) +
  geom_line(alpha = 0.7, show.legend = FALSE) +
  theme(text = element_text(size = 12, colour = "#A60000"), #family = "Segoe UI", 
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 6)) +
  labs(title = "Change in mobility to retail & recreation",
       subtitle = "Per cent change from baseline. Committee members/observers.",
       caption = "Source: Google Mobility",
       x = 'Date',
       y = '% change from baseline')

plot(BCBS_mobility)

BCBS_mobility_anim <- BCBS_mobility + 
  transition_reveal(date) +
  view_follow(fixed_y = TRUE)
BCBS_mobility_anim


# Google Mobility: country, state/province, municipality/county --------------------

# Example of country-level data: Germany (DE)
GoogleMobilityDataset %>% 
  filter(country_region_code == "DE") %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line()

# Example of state / province level: Argentinian Provinces, retail and recreation
GoogleMobilityDataset %>% 
  filter(country_region_code == "AR" & sub_region_1 != "" & sub_region_2 == "") %>% 
  ggplot(aes(x = date, 
             y = retail_and_recreation_percent_change_from_baseline,
             group = sub_region_1,
             colour = sub_region_1)) +
  geom_line()

# Example of municipality/county level: Málaga, Spain
GoogleMobilityDataset %>% 
  filter(country_region_code == "ES" & sub_region_1 != "" & sub_region_2 == "Málaga") %>% 
  melt(variable.name = "variable",
                               value.names = "mobility_change",
                               id.vars = c("sub_region_2", "date"),
                               measure.vars = c("retail_and_recreation_percent_change_from_baseline",
                                                "grocery_and_pharmacy_percent_change_from_baseline",
                                                "parks_percent_change_from_baseline",
                                                "transit_stations_percent_change_from_baseline",
                                                "workplaces_percent_change_from_baseline",
                                                "residential_percent_change_from_baseline")) %>% 
  ggplot(aes(x = date, 
             y = value,
             group = variable,
             colour = variable)) +
  geom_line()


# Association between mobility and consumer expenditure, US county level---------------


GoogleMobilityData_County <- data.table(read.csv(
  "https://github.com/OpportunityInsights/EconomicTracker/raw/main/data/Google%20Mobility%20-%20County%20-%20Daily.csv",
  stringsAsFactors = FALSE,
  na.strings = "."))

CardExpenditureData_County <- data.table(read.csv(
  "https://github.com/OpportunityInsights/EconomicTracker/raw/main/data/Affinity%20-%20County%20-%20Daily.csv",
  stringsAsFactors = FALSE,
  na.strings = "."))

CountyInfo <- data.table(read.csv("https://github.com/OpportunityInsights/EconomicTracker/raw/main/data/GeoIDs%20-%20County.csv"))

GoogleMobilityData_County[, 
                          Date := as.Date(paste(year, month, day, sep = "-"),
                                          format = "%Y-%m-%d")]
GoogleMobilityData_County[, ':='(year = NULL,
                                 month = NULL,
                                 day = NULL)]

CardExpenditureData_County[, 
                           Date := as.Date(paste(year, month, day, sep = "-"),
                                           format = "%Y-%m-%d")]

CardExpenditureData_County[, ':='(year = NULL,
                                  month = NULL,
                                  day = NULL)]

CountyDataset <- merge(GoogleMobilityData_County,
                       CardExpenditureData_County[Date >= "2020-02-24",],
                       by = c("countyfips", "Date"),
                       all.x = TRUE,
                       all.y = TRUE)

CountyDataset <- merge(CountyDataset, CountyInfo,
                       by = "countyfips")

CountyDataset$Date <- as.Date(CountyDataset$Date, format = "%Y-%m-%d")
rm(GoogleMobilityData_County, CardExpenditureData_County, CountyInfo)
counties_with_away_from_home_data <- unique(CountyDataset[!is.na(gps_away_from_home)]$countyfips)
CountyDataset <- CountyDataset[countyfips %in% counties_with_away_from_home_data,]

number_counties_sample <- length(unique(CountyDataset$countyfips))
population_counties_sample <- sum(CountyDataset[, mean(county_pop2019), by = countyfips]$V1)
end_2019_USA_pop <- 329131338 # source: https://www.census.gov/popclock/
perc_pop_counties_sample <- round(100 * population_counties_sample / end_2019_USA_pop, 1)

all_counties_anim_plot <- CountyDataset %>% 
  ggplot(aes(x = 100 * gps_away_from_home, y = 100 * spend_all,
             color = log(county_pop2019))) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.8) +
  scale_colour_gradientn(colours=rainbow(4)) + 
  # the animated part
  transition_time(Date) +
  ease_aes('linear') +
  enter_fade() +
  labs(title = 'Evolution of mobility and expenditure in US counties',
       subtitle = '{number_counties_sample} counties ({perc_pop_counties_sample}% of US pop.) - Date: {frame_time}',
       x = '% change in time away from home',
       y = '% change in expenditure',
       color = 'County population (log)',
       caption = 'Source: TrackTheRecovery.org')  +
  theme(text = element_text(size = 14, colour = "#A60000"), # element_text(family = "Segoe UI", size = 14, colour = "#A60000"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12))
animate(all_counties_anim_plot,
        duration = 25,
        fps = 30,
        start_pause = 5,
        end_pause = 10,
        width = 500,
        height = 400)


# Apple Mobility ----------------------------------------------------------
current_date <- as.Date(format(Sys.Date(), "%Y-%m-%d"))
date_apple_mobility <- current_date - 2
apple_mobility_url <- paste0("https://covid19-static.cdn-apple.com/covid19-mobility-data/2014HotfixDev12/v3/en-us/applemobilitytrends-",
                             date_apple_mobility,".csv")

apple_mobility_data <- data.table(read.csv(apple_mobility_url, 
                                           stringsAsFactors = FALSE))
# Unfortunately the following countries/regions below need to be filtered out
# of the curent code due to the presence of alternative names using scripts that
# are oriented from right to left (namely, Arabic and Hebrew scripts). These
# scripts interfere with the way the code reads the numeric data and I could not
# find a workaround that could be embedded in the graph. When the problem is
# solved I will upload a new version. Alternatively, users can download the
# original spreadsheet from the Apple Mobility website, manually remove the
# name in local script and try loading this into R by switching the command line
# above from the location in the internet to the location of the CSV file in
# the local computer.
apple_mobility_data <- apple_mobility_data %>% filter(!(region %in% c("Egypt",
                                                                     "Israel",
                                                                     "Morocco",
                                                                     "Saudi Arabia",
                                                                     "United Arab Emirates",
                                                                     "Cairo",
                                                                     "Casablanca",
                                                                     "Dubai",
                                                                     "Riyadh",
                                                                     "Tel Aviv",
                                                                     "Haifa District")))

apple_mobility_data <- apple_mobility_data %>% 
                        gather("date", "percent_change", 7:ncol(apple_mobility_data)) %>% 
                        mutate(date = as.Date(substr(date, 2, 11), format = "%Y.%m.%d")) %>% 
                        mutate(percent_change = as.numeric(percent_change))
# Note that Apple does not supply the values for 11 and 12 May 2020.


# exploring data from a specific country, for example Belgium:
country_apple_mob <- "Belgium"
apple_mobility_data %>% 
  filter(region == country_apple_mob & sub.region == "") %>% 
  ggplot(aes(x = date, y = percent_change, colour = transportation_type)) +
  geom_line() +
  labs(title = "Evolution of mobility by transport type",
       subtitle = country_apple_mob,
       y = '% change from baseline',
       color = "Transportation type",
       caption = 'Source: Apple Mobility')  +
  theme(text = element_text(size = 14, colour = "#A60000"), # element_text(family = "Segoe UI", size = 14, colour = "#A60000"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12))

# Facebook symptoms survey ------------------------------------------------

# Getting list of countries and regions for which University of Maryland (UMD) has
# calculated the symptom prevalence (for the US exclusively, Carnegie-Mellon University
# is the responsible university; everywhere else is done by UMD.)

url_countries <- "https://covidmap.umd.edu/api/country"
url_regions <- "https://covidmap.umd.edu/api/region"

list_of_countries <- fromJSON(content(GET(url_countries), as = "text", encoding = "UTF-8"), flatten = TRUE) %>% data.frame()
list_of_regions <- fromJSON(content(GET(url_regions), as = "text", encoding = "UTF-8"), flatten = TRUE) %>% data.frame()

# Using the example of Mexico

country_example = "Mexico"

# Retrieving all dates available for Mexico and its sub-regions
url_avail_dates <- paste("https://covidmap.umd.edu/api/datesavail?country=",
                         country_example,
                         sep = "")

avail_dates <- fromJSON(content(GET(url = url_avail_dates), as = "text", encoding = "UTF-8"), flatten = TRUE) %>%
  data.frame() %>% mutate(data.survey_date = as.Date(data.survey_date, format = "%Y%m%d"))
avail_dates <- range(avail_dates$data.survey_date)

url_api <- paste("https://covidmap.umd.edu/api/resources?indicator=covid&type=smoothed&country=",
                  country_example,
                 "&daterange=",
                 format(avail_dates[1], "%Y%m%d"), "-",
                 format(avail_dates[2], "%Y%m%d"),
                  sep = "")

coviddata <- fromJSON(content(GET(url = url_api), as = "text", encoding = "UTF-8"), flatten = TRUE) %>% data.frame()

coviddata %>% select(data.smoothed_cli, data.survey_date) 
