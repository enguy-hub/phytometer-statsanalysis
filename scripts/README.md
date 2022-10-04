# Guide for reading lm() output

1/ RSE: Lower is better
2/ Adjusted R-squared: Higher is better
3/ F-statistic p-value: Below 0.05 and more significant (*) is better


# ---- Assumptions in linear regression modelling ----

1/ In multiple regression, two or more predictors should NOT be related to each other, 
   so that one predictor can be used to predict the value of the other, and hence the name `independent variables`

2/ There is a linear relationship between the predictors (independent vars)
   and the outcome (residuals of response var)
   * Residual: Actual values - Fitted values (Predicted values) of response var

3/ Constant variance (homoscedasticity) of errors is another assumption of a linear regression model.
   The error terms may, for instance, change with the value of the response variable in case of 
   non-constant variance (heteroscedasticity) of errors.

4/ Consecutive error terms are UNcorrelated. The standard errors of the estimated 
   regression coefficients are calculated on the basis of this assumption
   * If the consecutive error terms are correlated, the standard errors 
   of the estimated regression coefficients may be much larger.


# Guide for model validation: checking linear regression assumptions

1/ Check multicollinearity among independent vars     
   + Use vif() function from the "car" package with the following syntax:
    > vif(`input_model`)

2/ Check for linear relationship between the predictors and the model outcome, by looking at:
  a/ Residual plot of "fitted values" vs the "residuals" of the model.
    + Use residualPlots() function from the "car" package with the following syntax:
     > residualPlots(`input_model`, type = "rstandard")
 
    * Explaining the output from running residualPlots():
      ~ Blue line: represents the "smooth" pattern between the fitted values (of response var) 
        and the standard residuals 
      --> The straighter and more aligned with the "zero" dashed line 
          the blue line is, the better/more linear the data is.

  b/ QQ plot of the residuals of fitted values to see if it is normally distributed
    + Use qqnorm() function from the "stats" package with the following syntax:
      > qqnorm(residuals(`input_model`))
      > qqline(residuals(`input_model`))
    + Double check with Shapiro test as follow:
      > shapiro.test(residuals(`input_model`))

  c/ Component Residual plots (CR plots) of "each predictor" vs the "residuals"
    + Use ceresPlots() function from the "car" package with the following syntax:
      > ceresPlots(`input_model`)
    * CR plot Ref: https://www.r-bloggers.com/2012/01/r-regression-diagnostics-part-1/
    * Explaining the output from running ceresPlot():
      ~ Pink line (residual line): represents the relationship between the predictor and residuals. 
      ~ Blue dashed line (component line): line of best fit. 
      --> Significant difference between the two lines for a predictor means that 
          that predictor and the outcome (residuals of response var) don’t have a linear relationship.
      !!! To fix this type of inconsistency, one could introduce a non-linear transformation 
          to the "inconsistent predictor" and save it as a new model, with the following syntax:
          > `new_model_name` <- update(`old_model`, .~.+I(`inconsistent predictor`^1.25))

  e/ Repeat steps 2a, 2b, and 2c to the newly "non-linear transformed" model to see if there is an improvement

3/ Perform anova() test the new model against the old model, with the following syntax:
  > anova(`new_model`, `old_model`, test = "F")

4/ Testing the constant variance (homoscedasticity) of errors using the Breusch-Pagan Test, 
   with the following syntax:
  > ncvTest('input_model')
   H0: Constant variance of errors (p >= 0.05)
   H1: Error variance changes with the level of the response or with a linear combination of predictors (p < 0.05)

5/ Testing the correlation of error terms, with the following syntax:
  > durbinWatsonTest('input_model')
   H0: The consecutive errors have NO auto-correlation (p >= 0.05)
   H1: The consecutive errors have auto-correlation (p < 0.05)