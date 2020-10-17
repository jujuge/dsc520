library(readxl)
library(knitr)
df <- read_xlsx("week-7-housing.xlsx", sheet = "Sheet2")
head(df)
colnames(df)
#I removed these as they were not needed for analysis
df$prop_type <- NULL
df$addr_full <- NULL
df$postalctyn <- NULL
df$ctyname <- NULL
df$sitetype <- NULL

#2. Create two variables; one that will contain the variables Sale Price and Square Foot of Lot (same variables used from previous assignment on simple regression) and one that will contain Sale Price and several additional predictors of your choice. Explain the basis for your additional predictor selections.
df$sale_price = df$'Sale Price'
model <- lm(sale_price ~ sq_ft_lot -1, data = df)
summary(model)
#i picked square foot because that is very important when it comes to pricing
#r2 and ar2 as follows: .1358 and .1357 
# 14% of the observed variation can be explained by the model's inputs
df1 = lm(formula = df$sale_price ~ df$sq_ft_lot + 
     df$year_built + df$bedrooms)
summary(df1)
#Multiple R-squared:  0.1289,	Adjusted R-squared:  0.1287
#F-statistic: 634.2 on 3 and 12861 DF,  p-value: < 2.2e-16


sb1 = lm(scale(sale_price) ~ scale(sq_ft_lot), data=df)
sb2 = lm(scale(sale_price) ~ scale(bedrooms), data=df)
sb3 = lm(scale(sale_price) ~ scale(year_built), data=df)
print(sb1)
print(sb2)
print(sb3)
#year built has the best standarized beta meaning this has the biggest impact on the the I trialed
summary(sb1)
summary(sb2)
summary(sb3)
#confident interval 90% will fall into outpputted range
confint(df1, level = 0.90)
#5 %          95 %
# (Intercept)   -1.249322e+07 -1.121353e+07
#df$sq_ft_lot   9.303273e-01  1.124654e+00
#df$year_built  5.773115e+03  6.414933e+03
#df$bedrooms    9.316897e+04  1.056824e+05

library(stats)
aov(model)
anova(model)
aov(df1)
anova(df1)

#aov(model)
#Call:
#  aov(formula = model)

#Terms:
#  sq_ft_lot    Residuals
#Sum of Squares  1.048222e+15 6.671880e+15
#Deg. of Freedom            1        12864

#Residual standard error: 720171.8
#Estimated effects are balanced
#> anova(model)
#Analysis of Variance Table

#Response: sale_price
#Df     Sum Sq    Mean Sq F value    Pr(>F)    
#sq_ft_lot     1 1.0482e+15 1.0482e+15  2021.1 < 2.2e-16 ***
#  Residuals 12864 6.6719e+15 5.1865e+11                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> aov(df1)
#Call:
#  aov(formula = df1)
#
#Terms:
#  df$sq_ft_lot df$year_built  df$bedrooms
#Sum of Squares  3.019674e+13  1.435435e+14 9.736062e+13
#Deg. of Freedom            1             1            1
#Residuals
#Sum of Squares  1.832473e+15
#Deg. of Freedom        12861

#Residual standard error: 377469.1
#Estimated effects may be unbalanced
#> anova(df1)
#Analysis of Variance Table
#
#Response: df$sale_price
#Df     Sum Sq    Mean Sq F value    Pr(>F)    
#df$sq_ft_lot      1 3.0197e+13 3.0197e+13  211.93 < 2.2e-16 ***
#  df$year_built     1 1.4354e+14 1.4354e+14 1007.44 < 2.2e-16 ***
#  df$bedrooms       1 9.7361e+13 9.7361e+13  683.31 < 2.2e-16 ***
#  Residuals     12861 1.8325e+15 1.4248e+11                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> results not certain
#
#g. Perform casewise diagnostics to identify outliers and/or influential cases, storing each function's output in a dataframe assigned to a unique variable name
df3 <- data.frame(standardized_residuals=rstandard(df1),
                 studentized_resisduals=rstudent(df1),
                 df_betas=dfbeta(df1),
                 leverage=hatvalues(df1))

#h. Calculate the standardized residuals using the appropriate command, specifying those that are +-2, storing the results of large residuals in a variable you create.
large_residuals <- function(value){
  if( abs(value) > 2){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

df3$large_residuals <- sapply(df$standardized_residuals,large_residuals)

#k. Investigate further by calculating the leverage, cooks distance, and covariance rations. Comment on all cases that are problematics.
df3$cooks_distance <- cooks.distance(df1)
lev <- hatvalues(df1)

#l. Perform the necessary calculations to assess the assumption of independence and state if the condition is met or not.
library(lmtest)
dwtest(formula = df1)

#m. Perform the necessary calculations to assess the assumption of no multicollinearity and state if the condition is met or not.
library(regclass)

kable(VIF(df1)
      #output is around 1 so there shouldn't be much bias
plot(df1)
hist(df1)

#o. Overall, is this regression model unbiased? If an unbiased regression model, what does this tell us about the sample vs. the entire population model?
#This model seems pretty ubniased
