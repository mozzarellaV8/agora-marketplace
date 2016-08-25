# Agora Marketplace Analysis
# Poisson Model on Counts


# load -----------------------------------------------------------------------

# count data
p <- read.csv("data/counts/crawl-distribution.csv")
str(p)
p$date <- as.Date(p$date)
p$vendor <- as.integer(p$vendor)

# raw data to aggregate
p2014 <- fread("~/GitHub/agora-data/agora-2014.csv", verbose = T, 
               stringsAsFactors = F)

# linear model 01 -------------------------------------------------------------

# potentially spurious due to time series, market volatility
pd.lm01 <- ggplot(p, aes(date, p)) + 
  theme_minimal() +
  geom_point(aes(color = p), size = 4.75, shape = 17) +
  ggtitle("Agora Marketplace: Number of Product Listings ~ Date") +
  theme(plot.title = element_text(family = "Times", face = "bold", size = 18)) +
  labs(x = "Date", y = "number of product listings (pages)") +
  theme(axis.title.x = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.text.x = element_text(family = "Times", face = "plain", size = 11)) +
  theme(axis.text.y = element_text(family = "Times", face = "plain", size = 11)) +
  theme(plot.margin = unit(c(3, 3, 3, 3), "cm"))

pd.lm01 + stat_smooth(method = lm, level = 0.95, se = FALSE, colour = "#CD2626",
                      linetype = "dashed") +
  theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0))) + 
  theme(axis.title.x = element_text(margin = margin(40, 0, 0, 0)))

# Poisson model on Product Counts by date -------------------------------------

pd.glm01 <- glm(p ~ date, family = "poisson", data = p)
summary(pd.glm01)
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -187.420   -59.783    -0.658    53.341    93.563 

#                     Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)     -3.319e+01  8.372e-02  -396.4   <2e-16 ***
#  date             2.593e-03  5.090e-06   509.5   <2e-16 ***

#     Null deviance: 1214565  on 202  degrees of freedom
# Residual deviance:  932944  on 201  degrees of freedom
# AIC: 935136

# That Z-Value is through the roof. 

options(scipen = 4)
pd.glm02 <- glm(p ~ date + vendor, family = "poisson", data = p)
summary(pd.glm02)
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -223.648   -38.573    -1.081    30.056   143.665 

#                     Estimate Std. Error    z value Pr(>|z|)    
#   (Intercept) -28.620540551   0.087994160  -325.3   <2e-16 ***
#   date          0.002283534   0.000005363   425.8   <2e-16 ***
#   vendor        0.005016896   0.000012577   398.9   <2e-16 ***

#     Null deviance: 1214565  on 202  degrees of freedom
# Residual deviance:  768506  on 200  degrees of freedom
# AIC: 770700

# So the residual deviance is 4000x the number of degrees of freedom in model 2,
# and about 4600x the number of degrees of freedom in model 1. 

par(mfrow = c(2, 2), mar = c(6, 6, 6, 6), family = "FranklinGothicSSK")
plot(pd.glm01, main = "Poisson Model - product count ~ date")
plot(pd.glm02, main = "Poisson Model - product count ~ date")

# Autocorrelation Function ----------------------------------------------------

library(zoo)
p.ts <- ts(p, frequency = 52, start = c(2014, 1), end = c(2014, 12))
p.ts <- ts(p$date, frequency = 52, start = c(2014, 1), end = c(2014, 12))
summary(p.ts)
class(p.ts)

acf(p.ts, plot = F)
pacf(p.ts, plot = F)

p.ts.ar <- ar(p.ts)
arima(p.ts)
# very opaque.

# Poisson ---------------------------------------------------------------------




