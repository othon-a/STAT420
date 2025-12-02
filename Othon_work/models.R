library(readr)
df = read_csv("cleaned_yield_with_gdp_cat.csv")
df_potatoes = subset(df, crop == "Potatoes")

subregions_add = lm(yield_hg_ha ~ year + rain_mm + pesticides_t + temp_c + gdp_tier + subregion, data = df_potatoes)
summary(lin_model)

gdp_int = lm(yield_hg_ha ~ year + rain_mm + pesticides_t + temp_c + gdp_tier + gdp_tier:year + gdp_tier:rain_mm + gdp_tier:pesticides_t + gdp_tier:temp_c, data = df_potatoes)
summary(gdp_int)

lin_int = lm(yield_hg_ha ~ year * rain_mm * pesticides_t * temp_c * gdp_tier, data = df_potatoes)
summary(lin_int)
