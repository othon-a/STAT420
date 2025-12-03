library(readr)

df = read_csv("cleaned_yield_with_gdp_cat.csv")
df_potatoes = subset(df, crop == "Potatoes")
df_potatoes$rain_mm_cent = df_potatoes$rain_mm - mean(df_potatoes$rain_mm)
df_potatoes$temp_c_cent = df_potatoes$temp_c - mean(df_potatoes$temp_c)
df_potatoes$year_cent = df_potatoes$year - mean(df_potatoes$year)

add_model = lm(yield_hg_ha ~ rain_mm + temp_c + pesticides_t + year + gdp_tier + , , data = df_potatoes)
summary(add_model)

model2 = lm(yield_hg_ha ~ rain_mm_cent * temp_c_cent + I(rain_mm_cent ^ 2) + I(temp_c_cent ^ 2) + year_cent + pesticides_t + gdp_tier + subregion, data = df_potatoes)
summary(model2)