
# Poisson without Date class

``` {r}
Call:
glm(formula = count ~ month, family = "poisson", data = mo)

Deviance Residuals: 
 [1]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0

Coefficients:
                Estimate Std. Error z value            Pr(>|z|)    
(Intercept)      8.98545    0.01119  802.98 <0.0000000000000002 ***
month2014-02-01  1.22920    0.01272   96.62 <0.0000000000000002 ***
month2014-03-01  0.85654    0.01336   64.13 <0.0000000000000002 ***
month2014-04-01  0.91804    0.01324   69.35 <0.0000000000000002 ***
month2014-05-01  1.67264    0.01220  137.15 <0.0000000000000002 ***
month2014-06-01  1.15792    0.01283   90.27 <0.0000000000000002 ***
month2014-07-01  1.61091    0.01226  131.43 <0.0000000000000002 ***
month2014-08-01  1.62275    0.01224  132.53 <0.0000000000000002 ***
month2014-09-01  2.20818    0.01179  187.31 <0.0000000000000002 ***
month2014-10-01  3.15391    0.01143  276.02 <0.0000000000000002 ***
month2014-11-01  3.54281    0.01135  312.12 <0.0000000000000002 ***
month2014-12-01  3.54625    0.01135  312.44 <0.0000000000000002 ***
month2015-01-01  3.47973    0.01136  306.28 <0.0000000000000002 ***
month2015-02-01  3.38638    0.01138  297.63 <0.0000000000000002 ***
month2015-03-01  3.30051    0.01139  289.66 <0.0000000000000002 ***

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 1486624.311215206980705  on 14  degrees of freedom
Residual deviance:       0.000000000050737  on  0  degrees of freedom
AIC: 224.04
```

Interesting - this output is different from the initial 2014 Poisson model in that it gave me coefficients for every month value. Judging by the coef and residual deviance, it looks like it's basically replicating the observed values? 

Is this because the object is of class 'data.table' rather than 'data.frame'?
No - this is because month is a `factor`. SMH.


# Poisson with Date

``` {r}
Call:
glm(formula = count ~ month, family = "poisson", data = mo)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-238.62   -96.77   -78.49    47.64   292.30  

Coefficients:
                  Estimate     Std. Error z value            Pr(>|z|)    
(Intercept) -103.139238078    0.118196967  -872.6 <0.0000000000000002 ***
month          0.007026223    0.000007212   974.2 <0.0000000000000002 ***

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 1486624  on 14  degrees of freedom
Residual deviance:  298182  on 13  degrees of freedom
AIC: 298380

Number of Fisher Scoring iterations: 4
```

Looking at the null and residual deviances, it looks like we have overdispersion. The null deviance is almost 5x that of the residual - so a strong effect is observed in the model. Let's try a quasipoisson fit with link = 'log'.

# Quasipoisson

```{r}
Call:
glm(formula = count ~ month, family = quasipoisson(link = "log"), 
    data = mo)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-238.62   -96.77   -78.49    47.64   292.30  

Coefficients:
               Estimate  Std. Error t value  Pr(>|t|)    
(Intercept) -103.139238   18.315131  -5.631 0.0000818 ***
month          0.007026    0.001118   6.287 0.0000280 ***

(Dispersion parameter for quasipoisson family taken to be 24010.84)

    Null deviance: 1486624  on 14  degrees of freedom
Residual deviance:  298182  on 13  degrees of freedom
AIC: NA

Number of Fisher Scoring iterations: 4
```

The null/residual deviance values are identical, but we now have a t-value instead of z.

# Linear Model comparison

``` {r}
Call:
lm(formula = count ~ month, data = mo)

Residuals:
   Min     1Q Median     3Q    Max 
-75868 -48532  -8941  36764  94838 

Coefficients:
             Estimate Std. Error t value  Pr(>|t|)    
(Intercept) -11362698    1725395  -6.586 0.0000175 ***
month             705        106   6.653 0.0000158 ***


Residual standard error: 53960 on 13 degrees of freedom
Multiple R-squared:  0.773,	Adjusted R-squared:  0.7555 
F-statistic: 44.27 on 1 and 13 DF,  p-value: 0.0000158
```

Using a linear model on count data isn't the right idea - for one thing, negative values are returned. But fitting one anyway can serve as a nice comparison tool for looking at the Poisson regression.

```{r}
sqrt(0.773) # 0.8792042
sse.lm01 <- sqrt(sum(lm01$residuals^2)) # 194570.3
mse.lm01 <- mean(residuals(lm01)^2)     # 2523840022
rmse.lm01 <- sqrt(mse.lm01)             # 50237.83
rss.lm01 <- sum(residuals(lm01)^2)      # 37857600331
rse.lm01 <- sqrt(sum(residuals(lm01)^2) / lm01$df.residual) # 53964.09
```


# Comparison

When counts are aggregated by month, the Poisson fit shows curvature to the observed values - minimizing the residual deviance. When a Poisson model is fit to daily counts (3-4 moving average), the Poisson comes much closer to a linear fit. 
