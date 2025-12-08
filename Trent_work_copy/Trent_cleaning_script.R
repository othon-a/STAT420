library(tidyverse)
df = read_csv("yield_df.csv")

#remove index column
df = df[ , !(names(df) == "...1") ]

df = df %>%
  rename(
    country = Area,
    crop = Item,
    year = Year,
    yield_hg_ha = `hg/ha_yield`,
    rain_mm = average_rain_fall_mm_per_year,
    pesticides_t = pesticides_tonnes,
    temp_c = avg_temp
  )

# check for duplicates
df %>% duplicated() %>% sum()

df = distinct(df)

# check for extreme values
summary(df)
#brazil seemed to had a large usage for pesticides, however it is gradually increasing over the years.
#looks good

df %>% 
  group_by(crop) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)

#chose top 5 crops
df = subset(df, crop %in% c("Potatoes", "Maize", "Wheat", "Rice, paddy", "Soybeans"))
df_potatoes = subset(df, crop == "Potatoes")

#Countries filtering into 3 GPD tiers
# data from https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
# using 2002 (middle of the year data) as a way to rank them
clean_gdp_raw = 
  "API_NY.GDP.PCAP.CD_DS2_en_csv_v2_280671.csv" %>%
  read_csv(skip = 4) %>% 
  select(`Country Name`, `2002`) %>%
  filter(!is.na(`2002`)) %>%
  rename(country = "Country Name", gdp_2002 = '2002') %>%
  mutate(
    gdp_tier = cut(
      gdp_2002,
      breaks = quantile(gdp_2002, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Low", "Middle", "High"),
      include.lowest = TRUE
    )
  ) %>% #mannual adjust country name
  mutate(
    country = case_when(
      country == "Bahamas, The" ~ "Bahamas",
      country == "Egypt, Arab Rep." ~ "Egypt",
      country == "Turkiye" ~ "Turkey",
      TRUE ~ country
    )
  )

sum(clean_gdp_raw$gdp_tier == "High")

df_with_gdp <- df %>%
  left_join(
    clean_gdp_raw %>% select(country, gdp_tier),
    by = "country"
  )


sum(df_with_gdp$gdp_tier == "Middle", na.rm = TRUE)
sum(df_with_gdp$gdp_tier == "Low", na.rm = TRUE)
sum(df_with_gdp$gdp_tier == "High", na.rm = TRUE)
#pretty evenly distributed

country_with_NA = filter(df_with_gdp, is.na(gdp_tier))
uniq_country = unique(country_with_NA$country )
#luckily there were only 3 countries. it was due to the name mismatch.
#manually adjust in clean GDP

write_csv(df_with_gdp, "cleaned_yield_with_gdp_cat.csv")

