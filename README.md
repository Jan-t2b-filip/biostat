# Model Documentation
## Linear Model Summary

Call:
lm(formula = formula, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-32.277 -15.463  -1.737  15.537  38.661 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept) 31.3748489 24.8970337   1.260    0.211
C           -0.0008374  0.1344126  -0.006    0.995
D            0.1224875  0.1394967   0.878    0.382

Residual standard error: 19.76 on 97 degrees of freedom
Multiple R-squared:  0.008058,	Adjusted R-squared:  -0.01239 
F-statistic: 0.394 on 2 and 97 DF,  p-value: 0.6754

## Logistic Model Summary (E)

Call:
glm(formula = formula, family = binomial, data = data)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)  
(Intercept)  4.323315   2.658662   1.626   0.1039  
A            0.002885   0.010725   0.269   0.7879  
B            0.412921   0.418692   0.986   0.3240  
C            0.014006   0.014314   0.979   0.3278  
D           -0.031220   0.014978  -2.084   0.0371 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 136.66  on 99  degrees of freedom
Residual deviance: 130.70  on 95  degrees of freedom
AIC: 140.7

Number of Fisher Scoring iterations: 4

