library(readr)
library(lmtest)
library(car)
library(MASS)

# subset potatoes and center predictors
df = read_csv("cleaned_yield_with_gdp_cat.csv")
df_potatoes = subset(df, crop == "Potatoes")



# additive model
add_model = lm(yield_hg_ha ~ rain_mm + temp_c + pesticides_t + year + gdp_tier + subregion, data = df_potatoes)
summary(add_model)

# additive model without subregions
noreg_model = lm(yield_hg_ha ~ rain_mm + temp_c + pesticides_t + year + gdp_tier, data = df_potatoes)
summary(noreg_model)

# ANOVA test using no-subregions as null model to compare the full model
anova(noreg_model, add_model)



# pesticides predictor transformation
plot(df_potatoes$yield_hg_ha ~ df_potatoes$pesticides_t)
plot(df_potatoes$yield_hg_ha ~ I(log(df_potatoes$pesticides_t)))
plot(df_potatoes$yield_hg_ha ~ I(log(df_potatoes$pesticides_t)^2))



# testing two-way interactions (minus subregion)
int_model = lm(yield_hg_ha ~ (temp_c + year + log(pesticides_t) + I(log(pesticides_t)^2) + gdp_tier)^2 + subregion, data = df_potatoes)

# backward BIC on int_model
bic_int_model = step(int_model, k = log(length(fitted(int_model))))

bic_int_model = lm(yield_hg_ha ~ temp_c + year + log(pesticides_t) + I(log(pesticides_t)^2) + 
                     gdp_tier + subregion + temp_c:I(log(pesticides_t)^2) + temp_c:gdp_tier + 
                     year:I(log(pesticides_t)^2) + log(pesticides_t):I(log(pesticides_t)^2) + 
                     log(pesticides_t):gdp_tier + I(log(pesticides_t)^2):gdp_tier, data = df_potatoes)
vif(bic_int_model)

# further reduced model to minimize VIF's
bic_model_small_pre = lm(yield_hg_ha ~ temp_c + year + log(pesticides_t) + I(log(pesticides_t)^2) + 
                           gdp_tier + subregion + temp_c:gdp_tier, data = df_potatoes)
vif(bic_model_small_pre)

# uses boxcox to find optimal response transformation
bxcx_quad = boxcox(bic_model_small_pre)
bxcx_quad$x[which.max(bxcx_quad$y)]

# model with transformed response
bic_model_small = lm(yield_hg_ha^.667 ~ temp_c + year + log(pesticides_t) + I(log(pesticides_t)^2) + 
                       gdp_tier + subregion + temp_c:gdp_tier, data = df_potatoes)

# model diagnostics
vif(bic_model_small)
summary(bic_model_small)
bptest(bic_model_small)
shapiro.test(resid(bic_model_small))

# diagnostic visualizations
plot(resid(bic_model_small) ~ fitted(bic_model_small))
abline(h = 0)
bptest(bic_model_small)
qqnorm(resid(bic_model_small))
qqline(resid(bic_model_small))
shapiro.test(resid(bic_model_small))

# predicted vs actual
plot(fitted(bic_model_small) ~ I(df_potatoes$yield_hg_ha^.667))
abline(bic_model_small)



# alt rain model: higher adjusted r-squared, one additional predictor, lower Shapiro test p-value
bic_model_rain = lm(yield_hg_ha^.667 ~ rain_mm + temp_c + year + log(pesticides_t) + I(log(pesticides_t)^2) + 
                       gdp_tier + subregion + rain_mm:gdp_tier, data = df_potatoes)

# model diagnostics
summary(bic_model_rain)
vif(bic_model_rain)
shapiro.test(resid(bic_model_rain))

# diagnostic visualizations
plot(resid(bic_model_rain) ~ fitted(bic_model_rain))
abline(h = 0)
bptest(bic_model_rain)
qqnorm(resid(bic_model_rain))
qqline(resid(bic_model_rain))
shapiro.test(resid(bic_model_rain))

# predicted vs actual
plot(fitted(bic_model_rain) ~ I(df_potatoes$yield_hg_ha^.667))
abline(bic_model_rain)