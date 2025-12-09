library(readr)
library(lmtest)
library(car)
library(MASS)

diagnostic_tests = function(model) {
  
  # Basic quantities
  res = residuals(model)
  n = length(res)
  p = length(coef(model))  
  
  bp = bptest(model)
  
  shapiro = shapiro.test(res)
  
  loocv_rmse = c(
    RMSE        = sqrt(mean(residuals(model)^2)),
    LOOCV_RMSE  = sqrt(mean((resid(model)/(1 - hatvalues(model)))^2))
  )
  
  vif_values = car::vif(model)
  cat("\n=== Breusch-Pagan Test (Homoscedasticity) ===\n")
  print(bp)
  cat("Interpretation: p-value < 0.05 suggests heteroscedasticity.\n\n\n")
  
  cat("=== Shapiro-Wilk Test (Normality of Residuals) ===\n")
  print(shapiro)
  cat("Interpretation: p-value < 0.05 suggests non-normal residuals.\n\n\n")
  
  cat("=== Variance Inflation Factor (VIF) ===\n\n")
  print(vif_values)
  cat("\nInterpretation: VIF > 5 suggests moderate collinearity; VIF > 10 suggests high multicollinearity.\n\n\n")
  
  cat("=== LOOCV-RMSE ===\n\n")
  print(loocv_rmse)
  cat("\nInterpretation: LOOCV-RMSE significantly larger than RMSE may indicate under or over-fitting\n\n")
}

# subset potatoes and center predictors
df = read_csv("cleaned_yield_with_gdp_cat.csv")
df_potatoes = subset(df, crop == "Potatoes")
df_potatoes = df_potatoes[, -(1:2)]
View(df_potatoes)


# additive model
add_model = lm(yield_hg_ha ~ ., data = df_potatoes)
summary(add_model)
plot(fitted(add_model) ~ df_potatoes$yield_hg_ha)

# model diagnostics
summary(add_model)
car::vif(add_model)
shapiro.test(resid(add_model))
sqrt(mean(residuals(add_model)^2))
sqrt(mean((resid(add_model) / (1 - hatvalues(add_model))) ^ 2))

# diagnostic visualizations
plot(resid(add_model) ~ fitted(add_model))
abline(h = 0)
bptest(add_model)
qqnorm(resid(add_model))
qqline(resid(add_model))
shapiro.test(resid(add_model))

diagnostic_tests(add_model)
bp2 = bptest(add_model)
boxcox_lambda = function(model) {
  bxcx = boxcox(model)
  lambda = c("Box-Cox Lambda" = bxcx$x[which.max(bxcx$y)])
  print(lambda)
}
boxcox_lambda(add_model)

lambda = c("Box-Cox Lambda" = boxcox(add_model)$x[which.max(add_model$y)])
print(lambda)


# additive model without subregions
noreg_model = lm(yield_hg_ha ~ . - subregion, data = df_potatoes)
summary(noreg_model)

View(df_potatoes)

# ANOVA test using no-subregions as null model to compare the full model
anova(noreg_model, add_model)



# pesticides predictor transformation
par(mfrow = c(2, 2))
plot(df_potatoes$yield_hg_ha ~ df_potatoes$pesticides_t,
     main = "Yield vs Pesticides",
     xlab = "Pesticides (in Tonnes)",
     ylab = "Yield (hg/ha)")
plot1 = lm(df_potatoes$yield_hg_ha ~ df_potatoes$pesticides_t)
abline(plot1, col = "red")

plot(df_potatoes$yield_hg_ha ~ I(log(df_potatoes$pesticides_t)),
     main = "Yield vs Log Pesticides",
     xlab = "log(pesticides_t)",
     ylab = "Yield (hg/ha)")
plot2 = lm(df_potatoes$yield_hg_ha ~ I(log(df_potatoes$pesticides_t)))
abline(plot2, col = "red")

plot(df_potatoes$yield_hg_ha ~ I(log(df_potatoes$pesticides_t)^2),
     main = "Yield vs Log Pesticides Squared",
     xlab = "I(log(pesticides_t)^2)",
     ylab = "Yield (hg/ha)")
plot3 = lm(df_potatoes$yield_hg_ha ~ I(log(df_potatoes$pesticides_t)^2))
abline(plot3, col = "red")
par(mfrow = c(1, 1))



# temperature predictor transformation
par(mfrow = c(2, 2))
plot(df_potatoes$yield_hg_ha ~ df_potatoes$temp_c,
     main = "Yield vs Temperature",
     xlab = "Temperature (in Tonnes)",
     ylab = "Yield (hg/ha)")
temp1 = lm(df_potatoes$yield_hg_ha ~ df_potatoes$temp_c)
abline(temp1, col = "red")

plot(df_potatoes$yield_hg_ha ~ I(df_potatoes$temp_c^2),
     main = "Yield vs Temperature",
     xlab = "I(temp_c^2)",
     ylab = "Yield (hg/ha)")
temp2 = lm(df_potatoes$yield_hg_ha ~ I(df_potatoes$temp_c^2))
abline(temp2, col = "red")
par(mfrow = c(1, 1))



# correlations
par(mfrow = c(2, 2))
gdp_tier_ordered = factor(df_potatoes$gdp_tier, levels = c("Low", "Middle", "High"))
boxplot(df_potatoes$yield_hg_ha ~ gdp_tier_ordered,
        main = "Crop Yield vs Gross Demostic Product",
        xlab = "Gross Domestic Product",
        ylab = "Crop Yield (hg/ha)")

boxplot(df_potatoes$temp_c ~ gdp_tier_ordered,
        main = "Temperature vs Gross Domestic Product",
        xlab = "Gross Domestic Product",
        ylab = "Temperature (in Degrees Celsius)")

boxplot(df_potatoes$yield_hg_ha ~ df_potatoes$year,
        main = "Crop Yield vs Year",
        xlab = "Year",
        ylab = "Crop Yield (hg/ha)")
par(mfrow = c(1, 1))



# testing two-way interactions (minus subregion)
int_model = lm(yield_hg_ha ~ (temp_c + I(temp_c^2) + year + log(pesticides_t) + I(log(pesticides_t)^2) + gdp_tier)^2 + subregion, data = df_potatoes)

# backward BIC on int_model
bic_int_model = step(int_model, k = log(length(fitted(int_model))))

bic_int_model = lm(yield_hg_ha ~ temp_c + I(temp_c^2) + year + log(pesticides_t) + 
                     I(log(pesticides_t)^2) + gdp_tier + subregion + temp_c:I(temp_c^2) + 
                     temp_c:year + temp_c:I(log(pesticides_t)^2) + temp_c:gdp_tier + 
                     I(temp_c^2):year + I(temp_c^2):gdp_tier + year:I(log(pesticides_t)^2) + 
                     log(pesticides_t):I(log(pesticides_t)^2) + log(pesticides_t):gdp_tier + 
                     I(log(pesticides_t)^2):gdp_tier, data = df_potatoes)
vif(bic_int_model)


bic_int_model = lm(yield_hg_ha ~ temp_c + I(temp_c^2) + year + log(pesticides_t) + 
                     I(log(pesticides_t)^2) + gdp_tier + subregion + temp_c:I(log(pesticides_t)^2) + temp_c:gdp_tier, data = df_potatoes)
car::vif(bic_int_model)



# further reduced model to minimize VIF's
bic_model_small_pre = lm(yield_hg_ha ~ temp_c + year + log(pesticides_t) + I(log(pesticides_t)^2) + 
                           gdp_tier + subregion + temp_c:gdp_tier, data = df_potatoes)
car::vif(bic_model_small_pre)

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
sqrt(mean((resid(bic_model_small) / (1 - hatvalues(bic_model_small))) ^ 2))

# diagnostic visualizations
plot(resid(bic_model_small) ~ fitted(bic_model_small))
abline(h = 0)
bptest(bic_model_small)
qqnorm(resid(bic_model_small))
qqline(resid(bic_model_small))
shapiro.test(resid(bic_model_small))

plot(fitted(bic_model_small) ~ I(df_potatoes$yield_hg_ha^.667),
     main = "Predicted vs. Actual",
     xlab = "Actual",
     ylab = "Predicted",
     col = "blue")
plot_model = lm(fitted(bic_model_small) ~ I(df_potatoes$yield_hg_ha^.667))
abline(plot_model, col = "orange", lwd = 2)



# alt rain model: higher adjusted r-squared, one additional predictor, lower Shapiro test p-value
bic_model_rain = lm(yield_hg_ha^.667 ~ rain_mm + temp_c + year + log(pesticides_t) + I(log(pesticides_t)^2) + 
                       gdp_tier + subregion + rain_mm:gdp_tier, data = df_potatoes)

# model diagnostics
summary(bic_model_rain)
vif(bic_model_rain)
shapiro.test(resid(bic_model_rain))
sqrt(mean((resid(bic_model_rain) / (1 - hatvalues(bic_model_rain))) ^ 2))

# diagnostic visualizations
plot(resid(bic_model_rain) ~ fitted(bic_model_rain))
abline(h = 0)
bptest(bic_model_rain)
qqnorm(resid(bic_model_rain))
qqline(resid(bic_model_rain))
shapiro.test(resid(bic_model_rain))

# predicted vs actual

plot(fitted(bic_model_rain) ~ I(df_potatoes$yield_hg_ha^.667),
     main = "Predicted vs. Actual",
     xlab = "Actual",
     ylab = "Predicted",
     col = "blue")
plot_model = lm(fitted(bic_model_rain) ~ I(df_potatoes$yield_hg_ha^.667))
abline(plot_model, col = "orange", lwd = 2)


bic_large_model = lm(yield_hg_ha^.667 ~ rain_mm + temp_c + year + log(pesticides_t) + I(log(pesticides_t)^2) + 
                      gdp_tier + subregion + temp_c:gdp_tier + rain_mm:gdp_tier, data = df_potatoes)

anova(bic_model_rain, bic_large_model)

anova(bic_model_small, bic_large_model)





int_y_x_trans_model_potato = lm(yield_hg_ha ~ year + (rain_mm + I((log(pesticides_t))^2) + I(temp_c^2) + gdp_tier)^2 +subregion , data = df_potatoes)
int_y_x_trans_model_potato_back_bic = step(int_y_x_trans_model_potato, direction = "backward", k=log(length(resid(int_y_x_trans_model_potato))))
bxcx_quad = boxcox(int_y_x_trans_model_potato_back_bic)
bxcx_quad$x[which.max(bxcx_quad$y)]
final_model = lm(yield_hg_ha^.707 ~ year + rain_mm + I((log(pesticides_t))^2) + I(temp_c^2) + 
                   gdp_tier + subregion + rain_mm:I((log(pesticides_t))^2) + 
                   rain_mm:gdp_tier + I((log(pesticides_t))^2):I(temp_c^2) + 
                   I((log(pesticides_t))^2):gdp_tier, data = df_potatoes)
summary(final_model)
car::vif(final_model)
shapiro.test(resid(final_model))
sqrt(mean(residuals(int_y_x_trans_model_potato_back_bic)^2))
sqrt(mean((resid(int_y_x_trans_model_potato_back_bic) / (1 - hatvalues(int_y_x_trans_model_potato_back_bic))) ^ 2))