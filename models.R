library(readr)
library(lmtest)
library(car)

# subset potatoes and center predictors
df = read_csv("cleaned_yield_with_gdp_cat.csv")
df_potatoes = subset(df, crop == "Potatoes")
df_potatoes$rain_mm_cent = df_potatoes$rain_mm - mean(df_potatoes$rain_mm)
df_potatoes$temp_c_cent = df_potatoes$temp_c - mean(df_potatoes$temp_c)
df_potatoes$year_cent = df_potatoes$year - mean(df_potatoes$year)

# additive model
add_model = lm(yield_hg_ha ~ rain_mm + temp_c + pesticides_t + year + gdp_tier + subregion, , data = df_potatoes)
summary(add_model)

# test polynomial models of different degrees
poly_model = lm(yield_hg_ha ~ poly(rain_mm_cent, 2) + poly(temp_c_cent, 2) + year_cent + pesticides_t + gdp_tier + subregion, data = df_potatoes)
summary(poly_model)

# polynomial model diagnostics
plot(resid(poly_model) ~ fitted(poly_model))
abline(h = 0)
bptest(poly_model)
qqnorm(resid(poly_model))
qqline(resid(poly_model))
shapiro.test(resid(poly_model))

# the quadratic model featuring subregions attains a high r-squared and somewhat reasonable normality
quad_model = lm(yield_hg_ha ~ rain_mm_cent + temp_c_cent + I(rain_mm_cent ^ 2) + I(temp_c_cent ^ 2) + year_cent + pesticides_t + gdp_tier + subregion, data = df_potatoes)
summary(quad_model)

quad_model_no_ctr = lm(yield_hg_ha ~ rain_mm + temp_c + I(rain_mm ^ 2) + I(temp_c ^ 2) + year_cent + pesticides_t + gdp_tier + subregion, data = df_potatoes)
summary(quad_model_no_ctr)

# however, we see issues with collinearity
# still, it seems to be a decent model for prediction
vif(quad_model)

# we can remove collinarity issues by removing subregions from the model
# however, r-squared and other diagnostics suffer
nosubreg_quad = lm(yield_hg_ha ~ rain_mm_cent * temp_c_cent + I(rain_mm_cent ^ 2) + I(temp_c_cent ^ 2) + year_cent + pesticides_t + gdp_tier, data = df_potatoes)
summary(nosubreg_quad)
vif(nosubreg_quad)

plot(resid(nosubreg_quad) ~ fitted(nosubreg_quad))
abline(h = 0)
bptest(nosubreg_quad)
qqnorm(resid(nosubreg_quad))
qqline(resid(nosubreg_quad))
shapiro.test(resid(nosubreg_quad))
