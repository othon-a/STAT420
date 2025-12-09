library(readr)
library(lmtest)
library(MASS)
library(faraway)
library(ppcor)
library(tidyverse)
library(glmnet)

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
       breaks = 15,
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

#---------------------initial inspection --------------------
#had to convert categorical to numeric to use pairs and cor
df_potatoes_num = df_potatoes%>%        
  mutate(
    gdp_tier = as.numeric(factor(gdp_tier, levels = c("Low", "Middle", "High"))),     
    subregion = as.numeric(factor(subregion))    
  )

pairs(df_potatoes_num)
round(cor(df_potatoes_num), 2)
# did not see any obvious high covariance and linearity between predictors


#---------------------- full model inspection---------------------------
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


#--------------------Transformation-----------------------------------

y_trans_full_model = lm(yield_hg_ha^0.667 ~ . , data = df_potatoes) #not exactly the same as the book
summary(y_trans_full_model)

diagnostic_plots(y_trans_full_model)
diagnostic_tests(y_trans_full_model)

y_trans_full_model_nore = lm(yield_hg_ha^0.5 ~ . -subregion , data = df_potatoes)
summary(y_trans_full_model_nore)

diagnostic_plots(y_trans_full_model_nore)
diagnostic_tests(y_trans_full_model_nore)


#compare the transformed predictor and response vs non transformed
par(mfrow = c(2, 2))

plot(df_potatoes$pesticides_t, df_potatoes$yield_hg_ha, xlab = "pesticides_t", ylab = "yield_hg_ha",  main = "Raw: yield vs pesticides")

x_pest = (log(df_potatoes$pesticides_t))^2
y_trans = df_potatoes$yield_hg_ha^0.5

plot(  x_pest,  y_trans,  xlab = "(log(pesticides_t))^2",  ylab = "sqrt(yield_hg_ha)",  main = "Transformed: sqrt(yield) vs (log(pest))^2")

plot(df_potatoes$temp_c, df_potatoes$yield_hg_ha, xlab = "temp_c", ylab = "yield_hg_ha", main = "Raw: yield vs temp_c")

x_temp = (df_potatoes$temp_c)^2

plot(x_temp, y_trans, xlab = "temp_c^2", ylab = "sqrt(yield_hg_ha)", main = "Transformed: sqrt(yield) vs temp_c^2")
#looks more linear now after transformation

# best model so far without interaction and with subregion
y_x_trans_model_potato = lm(yield_hg_ha^0.667 ~ year + rain_mm + I((log(pesticides_t))^2) + I(temp_c^2) + gdp_tier +subregion , data = df_potatoes)
summary(y_x_trans_model_potato)

diagnostic_plots(y_x_trans_model_potato)
diagnostic_tests(y_x_trans_model_potato)

y_x_trans_model_potato_back_bic = step(y_x_trans_model_potato, direction = "backward", k=log(length(resid(y_x_trans_model_potato))))
# lol the original is already the best.

# best model so far without interaction and without subregion

y_x_trans_model_potato_no_re = lm(yield_hg_ha^0.5 ~ year + rain_mm + I((log(pesticides_t))^2) + I(temp_c^2) + gdp_tier , data = df_potatoes)
summary(y_x_trans_model_potato_no_re)

diagnostic_plots(y_x_trans_model_potato_no_re)
diagnostic_tests(y_x_trans_model_potato_no_re)

y_x_trans_model_potato_no_re_back_bic = step(y_x_trans_model_potato_no_re, direction = "backward", k=log(length(resid(y_x_trans_model_potato_no_re))))
#rain is removed

#---------------------Testing interaction--------------
#

#removed year compare to original
int_y_x_trans_model_potato = lm(yield_hg_ha^0.667 ~ year + (rain_mm + I((log(pesticides_t))^2) + I(temp_c^2) + gdp_tier)^2 +subregion , data = df_potatoes)

int_y_x_trans_model_potato_back_bic = step(int_y_x_trans_model_potato, direction = "backward", k=log(length(resid(int_y_x_trans_model_potato))))
summary(int_y_x_trans_model_potato_back_bic)

diagnostic_plots(int_y_x_trans_model_potato_back_bic)
diagnostic_tests(int_y_x_trans_model_potato_back_bic)
#remove year because it inflated with log^2 pesticide term

# Actual vs Predicted (transformed scale)
y_actual = df_potatoes$yield_hg_ha^0.667
y_pred = predict(int_y_x_trans_model_potato_back_bic)

par(mfrow = c(1, 1))
plot(
  y_actual, y_pred,
  xlab = "Actual (yield_hg_ha^0.6)",
  ylab = "Predicted (yield_hg_ha^0.6)",
  main = "Actual vs Predicted (Transformed Scale)",
  pch = 19, col = "blue"
)
abline(0, 1, col = "red", lwd = 2)

# Actual vs Predicted (original yield scale)
y_actual_orig = df_potatoes$yield_hg_ha
y_pred_trans = predict(int_y_x_trans_model_potato_back_bic)
y_pred_orig = y_pred_trans^(1/0.667)

par(mfrow = c(1, 1))
plot(
  y_actual_orig, y_pred_orig,
  xlab = "Actual yield_hg_ha",
  ylab = "Predicted yield_hg_ha",
  main = "Actual vs Predicted (Original Yield Scale)",
  pch = 19, col = "darkgreen"
)
abline(0, 1, col = "red", lwd = 2)


# Interactive model without region
int_y_x_trans_model_potato_nore = lm(yield_hg_ha^0.5 ~ year + (rain_mm + I((log(pesticides_t))^2) + I(temp_c^2) + gdp_tier)^2 , data = df_potatoes)

int_y_x_trans_model_potato_nore_back_bic = step(int_y_x_trans_model_potato_nore, direction = "backward", k=log(length(resid(int_y_x_trans_model_potato_nore))))
summary(int_y_x_trans_model_potato_nore_back_bic)

diagnostic_plots(int_y_x_trans_model_potato_nore_back_bic)
diagnostic_tests(int_y_x_trans_model_potato_nore_back_bic)


# Actual vs predicted on transformed scale (sqrt(y))
y_actual = df_potatoes$yield_hg_ha^0.5
y_pred = predict(int_y_x_trans_model_potato_nore_back_bic)

par(mfrow = c(1, 1))
plot(  y_actual, y_pred,  xlab = "Actual (yield_hg_ha^0.5)",  ylab = "Predicted (yield_hg_ha^0.5)",  main = "Actual vs Predicted (Transformed Scale: sqrt(yield))",  pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

# Convert predicted values back to original scale
y_actual_orig = df_potatoes$yield_hg_ha
y_pred_trans = predict(int_y_x_trans_model_potato_nore_back_bic)
y_pred_orig = (y_pred_trans)^2     # because inverse of sqrt(y) is (sqrt(y))^2

par(mfrow = c(1, 1))
plot(  y_actual_orig, y_pred_orig,  xlab = "Actual yield_hg_ha",  ylab = "Predicted yield_hg_ha",  main = "Actual vs Predicted (Original Yield Scale)",  pch = 19, col = "darkgreen")
abline(0, 1, col = "red", lwd = 2)


#---------lasso regression to prioritize the explanatory predictors---------

# ============================
# LASSO with region variables
# ============================

run_lasso_scaled = function(x, y, family = "gaussian", alpha = 1, seed = 420, do_plots = TRUE) {
  
  x_scaled = scale(x)
  y_scaled = scale(y)

  set.seed(seed)
  lasso_fit = glmnet(x = x_scaled, y = y_scaled, family = family, alpha = alpha, standardize = FALSE)
  
  coef_paths = coef(lasso_fit)
  coef_df = as.data.frame(as.matrix(coef_paths))
  
  set.seed(seed)
  cv_fit = cv.glmnet(x = x_scaled, y = y_scaled, family = family, alpha = alpha, standardize = FALSE )
  
  coef_min = coef(cv_fit, s = "lambda.min")
  coef_1se = coef(cv_fit, s = "lambda.1se")
  
  if (do_plots) {
    par(mfrow = c(2, 1),mar = c(5, 4, 6, 2))
    plot(lasso_fit, label = TRUE, main = "LASSO Coefficient Paths")
    plot(cv_fit, main = "Cross-Validation Curve")
  }
  
  return(list(
    lasso_fit = lasso_fit,
    cv_fit = cv_fit,
    coef_df = coef_df,
    coef_min = coef_min,
    coef_1se = coef_1se
  ))
}

y_lasso = (df_potatoes$yield_hg_ha)^0.667
x_lasso = model.matrix((yield_hg_ha)^0.667 ~ year + (rain_mm + I((log(pesticides_t))^2) + I(temp_c^2) + gdp_tier)^2 +subregion, data = df_potatoes)[, -1]

par(mfrow = c(1, 1))
lasso_results = run_lasso_scaled(x_lasso, y_lasso)
plot(lasso_results$lasso_fit)
View(lasso_results$coef_df)

lasso_results$coef_1se
lasso_results$coef_min
lasso_results$lasso_fit

#taken out
# rain_mm:I(temp_c^2)                      .  
# I((log(pesticides_t))^2):gdp_tierMiddle  .           
# I(temp_c^2):gdp_tierLow                  .  


lasso_y_x_trans_model_potato = lm(yield_hg_ha^0.667 ~ year + rain_mm + I((log(pesticides_t))^2)    + gdp_tier +subregion + rain_mm:I((log(pesticides_t))^2)
                                +rain_mm:gdp_tier + I((log(pesticides_t))^2):I(temp_c^2), data = df_potatoes)

summary(lasso_y_x_trans_model_potato)
diagnostic_plots(lasso_y_x_trans_model_potato)
diagnostic_tests(lasso_y_x_trans_model_potato)

# ============================
# LASSO without region variables
# ============================
y_lasso_nore = (df_potatoes$yield_hg_ha)^0.5
x_lasso_nore = model.matrix(
  (yield_hg_ha)^0.5 ~ year + (rain_mm + I((log(pesticides_t))^2) + I(temp_c^2) + gdp_tier)^2,
  data = df_potatoes
)[, -1]

lasso_results_nore = run_lasso_scaled(x_lasso_nore, y_lasso_nore)

lasso_results_nore$coef_1se
lasso_results_nore$coef_min

#taken out
# rain_mm:I(temp_c^2)                      .  
# I((log(pesticides_t))^2):gdp_tierLow     .           
# I((log(pesticides_t))^2):gdp_tierMiddle  .           
# I(temp_c^2):gdp_tierLow                  .   
