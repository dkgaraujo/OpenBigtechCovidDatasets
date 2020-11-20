library(tidyverse)
library(pins)

containment_measures <- pin("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv") %>% 
  read_csv() %>% 
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"))

available_countries <- unique(containment_measures$CountryName)

BCBS_Americas <- c("Argentina",
                   "Brazil",
                   "Canada",
                   "Chile",
                   "Mexico",
                   "United States")

BCBS_Europe <- c("Belgium",
                 "Switzerland",
                 "Germany",
                 "Spain",
                 "France",
                 "United Kingdom",
                 "Italy",
                 "Luxembourg",
                 "Netherlands",
                 "Russia",
                 "Sweden")

BCBS_ROW <- c("United Arab Emirates",
              "China",
              "Hong Kong",
              "India",
              "Indonesia",
              "Japan",
              "Malaysia",
              "Saudi Arabia",
              "Singapore",
              "South Africa",
              "South Korea",
              "Turkey")

BCBS_all <- c(BCBS_Americas, BCBS_Europe, BCBS_ROW)

containment_measures <- containment_measures %>%
  filter(CountryName %in% BCBS_all & is.na(RegionCode)) %>% 
  select(-RegionName, -RegionCode) %>% 
  nest(!c(CountryName, CountryCode))

containment_measures <- containment_measures %>% 
  mutate(Region = case_when(
    CountryName %in% BCBS_Americas ~ "Americas",
    CountryName %in% BCBS_Europe ~ "Europe",
    CountryName %in% BCBS_ROW ~ "RoW",
    TRUE ~ NA_character_
  ))

# a function to calculate a composite index --------------------------------------------------------

containment_index <- function(containment_dataset) {
  tibble(Date = containment_dataset$Date,
         Index = 
          containment_dataset$'C1_School closing' +
          containment_dataset$'C2_Workplace closing' +
          containment_dataset$'C3_Cancel public events' +
          containment_dataset$'C4_Restrictions on gatherings' +
          containment_dataset$'C5_Close public transport' +
          containment_dataset$'C6_Stay at home requirements' +
          containment_dataset$'C7_Restrictions on internal movement' +
          containment_dataset$'C8_International travel controls'
    )
}


# calculates index --------------------------------------------------------

containment_measures <- containment_measures %>% 
  mutate(containment_index = map(data, ~ containment_index(.)))

# plotting ----------------------------------------------------------------

latest_date <- containment_measures  %>% 
  unnest(containment_index) %>% 
  pull(Date) %>% 
  max()

containment_measures  %>% 
  unnest(containment_index) %>% 
  filter(Date == max(Date))
  pull(Date) %>% 
  max()

containment_measures  %>% 
  unnest(containment_index) %>% 
  ggplot(aes(x = Date, y = Index, colour = Region)) +
  geom_smooth(alpha = 0.7, size = 2, se = FALSE) +
  geom_hline(yintercept = 0, colour = "black") +
  # customized_theme_file() +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month") +
  scale_colour_manual(values = unname(BIS_dark_colours_Neil_palette)) +
  labs(title = "Containment measures",
       subtitle = paste("Daily frequency. Data as of", latest_date),
       x = element_blank(),
       y = "Index",
       colour = element_blank())
