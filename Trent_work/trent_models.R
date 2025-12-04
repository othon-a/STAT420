library(readr)
library(lmtest)
library(MASS)
library(faraway)
library(ppcor)
library(tidyverse)

diagnostic_plots = function(model) {
  
  # 2x2 layout
  par(mfrow = c(2, 2))
  
  fit = fitted(model)
  res = residuals(model)
  
  plot(fit, res,
       xlab = "Fitted values",
       ylab = "Residuals",
       main = "Fitted vs Residuals")
  abline(h = 0, lty = 2, col = "red")
  
  hist(res,
       main = "Histogram of Residuals",
       xlab = "Residuals")
  
  qqnorm(res,
         main = "Normal Q-Q Plot")
  qqline(res, col = "red")
  
  boxcox(
    model,
    lambda = seq(-2, 2, 0.1),
    plotit = TRUE
  )
}

diagnostic_tests = function(model) {
  
  # Basic quantities
  res = residuals(model)
  n = length(res)
  p = length(coef(model))  
  
  bp = bptest(model)
  
  shapiro = shapiro.test(res)
  
  vif_values = car::vif(model)
  cat("=== Breusch-Pagan Test (Homoscedasticity) ===\n")
  print(bp)
  cat("\nInterpretation: p-value < 0.05 suggests heteroscedasticity.\n\n")
  
  cat("=== Shapiro-Wilk Test (Normality of Residuals) ===\n")
  print(shapiro)
  cat("\nInterpretation: p-value < 0.05 suggests non-normal residuals.\n\n")
  
  cat("=== Variance Inflation Factor (VIF) ===\n")
  print(vif_values)
  cat("\nInterpretation: VIF > 5 suggests moderate collinearity; VIF > 10 suggests high multicollinearity.\n\n")
  
  return(list(
    bp_test = bp,
    shapiro_test = shapiro,
    vif = vif_values,
    thresholds = list(
      vif = 5
    )
  ))
}

unusual_obs_tests = function(model) {
  h = hatvalues(model)
  lev_threshold = 2 * (p / n)  
  high_leverage_idx = which(h > lev_threshold)
  
  stud_res = rstudent(model)
  outlier_threshold = 2 
  outlier_idx = which(abs(stud_res) > outlier_threshold)
  
  cooks = cooks.distance(model)
  cooks_threshold = 4 / n 
  influential_idx = which(cooks > cooks_threshold)
  
  cat("=== High Leverage Points ===\n")
  cat("Leverage threshold:", round(lev_threshold, 3), "\n")
  if (length(high_leverage_idx) == 0) {
    cat("No high leverage points detected.\n\n")
  } else {
    cat("High leverage observations (indices):\n")
    print(high_leverage_idx)
    cat("\n")
  }
  
  cat("=== Outliers (Studentized Residuals) ===\n")
  cat("Outlier threshold: |rstudent| >", outlier_threshold, "\n")
  if (length(outlier_idx) == 0) {
    cat("No outliers detected.\n\n")
  } else {
    cat("Outlier observations (indices):\n")
    print(outlier_idx)
    cat("\n")
  }
  
  cat("=== Influential Points (Cook's Distance) ===\n")
  cat("Cook's D threshold:", round(cooks_threshold, 4), "\n")
  if (length(influential_idx) == 0) {
    cat("No influential points detected.\n\n")
  } else {
    cat("Influential observations (indices):\n")
    print(influential_idx)
    cat("\n")
  }
  return(list(  
  leverage = h,
  high_leverage_idx = high_leverage_idx,
  stud_residuals = stud_res,
  outlier_idx = outlier_idx,
  cooks_distance = cooks,
  influential_idx = influential_idx,
  thresholds = list(
    leverage = lev_threshold,
    outlier = outlier_threshold,
    cooks = cooks_threshold,
  )
  ))
}



df = read_csv("cleaned_yield_with_gdp_cat_region.csv")
df_potatoes = subset(df, crop == "Potatoes")

# get rid of unnessary info
df_potatoes = df_potatoes %>%
  select(-country, -crop)

#had to convert categorical to numeric to use pairs and cor
df_potatoes_num = df_potatoes%>%        
  mutate(
    gdp_tier = factor(gdp_tier, levels = c("Low", "Middle", "High")),     
    subregion = as.numeric(factor(subregion))    
  )

pairs(df_potatoes_num)
round(cor(df_potatoes_num), 2)
# did not see any obvious high covariance and linearity between predictors


#fit a full model first
full_model = lm(yield_hg_ha ~ . , data = df_potatoes)
summary(full_model)

diagnostic_plots(full_model)
diagnostic_tests(full_model)
#summary:

#subregion is controversion since it has divided dataset into very small pieces
full_model_nore = lm(yield_hg_ha ~ . -subregion , data = df_potatoes)
summary(full_model_nore)

diagnostic_plots(full_model_nore)
diagnostic_tests(full_model_nore)
#summary: the boxcox strongly suggest taking 



#-------------------------------------------------------

#Transformation
y_trans_full_model = lm(yield_hg_ha^0.6 ~ . , data = df_potatoes)
summary(y_trans_full_model)

diagnostic_plots(y_trans_full_model)
diagnostic_tests(y_trans_full_model)

y_trans_full_model_nore = lm(yield_hg_ha^0.5 ~ . -subregion , data = df_potatoes)
summary(y_trans_full_model_nore)

diagnostic_plots(y_trans_full_model_nore)
diagnostic_tests(y_trans_full_model_nore)

par(mfrow = c(1, 1))
plot(yield_hg_ha ~ pesticides_t ,data = df_potatoes)

x=(log(df_potatoes$pesticides_t))^2
plot(yield_hg_ha^0.5 ~ x, data = df_potatoes)

plot(yield_hg_ha ~ temp_c ,data = df_potatoes)

x2=(df_potatoes$temp_c)^2
plot(yield_hg_ha^0.5 ~ x2, data = df_potatoes)


y_x_trans_model_potato = lm(yield_hg_ha^0.6 ~ year + rain_mm + I((log(pesticides_t))^2) + I(temp_c^2) + gdp_tier +subregion , data = df_potatoes)
summary(y_x_trans_model_potato)

diagnostic_plots(y_x_trans_model_potato)
diagnostic_tests(y_x_trans_model_potato)





gdp_int = lm(yield_hg_ha ~ year + rain_mm + pesticides_t + temp_c + gdp_tier + gdp_tier:year + gdp_tier:rain_mm + gdp_tier:pesticides_t + gdp_tier:temp_c, data = df_potatoes)
summary(gdp_int)

lin_int = lm(yield_hg_ha ~ year * rain_mm * pesticides_t * temp_c * gdp_tier, data = df_potatoes)
summary(lin_int)

pot_model_add = lm(yield_hg_ha ~ year + rain_mm + pesticides_t + temp_c + gdp_tier, data = df_potatoes)
summary(pot_model_add )

 
