# Negative Binomial Regression

library(MASS)
library(data.table)
library(ggplot2)
library(broom)
library(extrafont)
library(extrafontdb)

ag <- fread("~/GitHub/agora-data/agora-02.csv", stringsAsFactors = T)
ag$date <- as.Date(ag$date)

dates <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "month")
dw <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "week")

# daily counts ----------------------------------------------------------------

jc <- as.data.frame(table(ag$j))
colnames(jc) <- c("julian.date", "count")
# write.csv(jc, file = "data/counts/juliandaily.csv", row.names = F)
jc$date <- unique(ag$date)
jc$log <- log(jc$count)
jc <- jc[c(3, 1, 2, 4)]

sum(jc$count) 
# 2322961

summary(jc$count)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       7    3559    7416    7567   10960   20440 

quantile(jc$count)
#    0%   25%   50%   75%  100% 
#     7  3559  7416 10963 20445

summary(jc$log)
plot(quantile(jc$log))

par(mfrow = c(2, 1), mar = c(6, 6, 6, 6), las = 1)
qqplot(jc$julian.date, jc$count, xlab = "Julian Date", ylab = "",
       main = "daily observed listing count ~ date")
qqplot(jc$julian.date, jc$log, xlab = "Julian Date", ylab = "(log) listing count",
       main = "(log) daily observed listing count ~ date")

# distribution of counts ------------------------------------------------------

par(mfrow = c(2, 1), mar = c(6, 6, 6, 6), las = 1)
hist(jc$count, breaks = 100, xlab = "number of listings", ylab = "Frequency",
     main = "daily observed listing counts")
hist(log(jc$count), breaks = 100, xlim = c(2, 12), 
     xlab = "(log) number of listings", ylab = "Frequency",
     main = "(log) daily observed listing counts")

# this looks like it could be zero inflated. the log count approaches normal.

# daily count histogram -------------------------------------------------------

dailyCount <- ggplot(jc, aes(count)) +  
  geom_histogram(binwidth = 300, color = "black", alpha = 0) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.text.y = element_text(size = 14.5),
        axis.text.x = element_text(size = 14.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(20, 0, 0, 0))) 

dailyCount + labs(title = "Daily Product Count Totals", 
                  x = "number of listings", y = "frequency of count")


# log daily count histogram ---------------------------------------------------

logDailyCount <-  ggplot(jc, aes(log)) +  
  geom_histogram(binwidth = 0.075, color = "black", alpha = 0) +
  scale_x_continuous(limits = c(2, 12), breaks = seq(2, 12, 1)) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.y = element_text(size = 14.5),
        axis.text.x = element_text(size = 14.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(20, 0, 0, 0))) 

logDailyCount + labs(title = "(log) Daily Product Count Totals", 
                     x = "log number of listings", 
                     y = "frequency of count")


# Observed Number of Listings by Date ----------------------------------------

oc1 <- ggplot(jc, aes(date, count, color = count)) + 
  geom_point(size = 4) +
  scale_x_date(breaks = dates, labels = dates) +
  theme_gray(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0),
                                    family = "Times New Roman",
                                    face = "italic")) +
  labs(title = "Observed Product Counts by Date", x = "", 
       y = "number of product listings")

# Negative Binomial 00 --------------------------------------------------------

# TWO PARAMETER (Crawley):

# variance-mean ratio
var(jc$count)/mean(jc$count) 
# 2998.922 - overdispersion

# parameter one: mean counts
mean(jc$count)
# 7566.648

# parameter two: clumping parameter 'k'
# measures degrees of aggregation in the data
# k < 1 is highly aggregated;
# k > 5 shows randomness. 
mean(jc$count)^2 / (var(jc$count) - mean(jc$count))
# 2.523964 - right in the middle of aggregated and random.

# expected frequency:
# 1. freq for which we want probability
# 2. number of successes
# 3. mean number of counts

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6), las = 1)
exp <- dnbinom(0:307, 30, mu = 7566.448) * 307
plot(exp)

# Negative Binomial 01 --------------------------------------------------------

# k = theta
k <- mean(jc$count)^2 / (var(jc$count) - mean(jc$count))
# 2.523964

nb01 <- glm.nb(count ~ date, data = jc, init.theta = 2.523964)
summary(nb01)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -6.4525809  5.8183226  -1.109  0.26743   
# date         0.0009358  0.0003540   2.643  0.00821 **

# (Dispersion parameter for Negative Binomial(1.5367) family taken to be 1)

# Null deviance: 344.30  on 306  degrees of freedom
# Residual deviance: 339.22  on 305  degrees of freedom
# AIC: 6070.4
# Number of Fisher Scoring iterations: 1
# Theta:  1.537 
# Std. Err.:  0.113 

# 2 x log-likelihood:  -6064.378 

tidy(nb01)
#          term      estimate    std.error statistic     p.value
# 1 (Intercept) -6.4525807535 5.8183226270 -1.109010 0.267425662
# 2        date  0.0009358194 0.0003540434  2.643234 0.008211832

nb01a <- augment(nb01)
nb01a$fitted.01 <- exp(nb01a$.fitted)
nb01a <- nb01a[c(1, 2, 10, 3, 4, 5, 6, 7, 8, 9)]

jc$fit.nb01 <- nb01a$fitted.01

# linear comparison
lm01 <- lm(count ~ date, data = jc)
summary(lm01)
jc$fit.lm01 <- lm01$fitted.values

# 1. nb assumes conditional means != conditional variances.
# 2. inequality captured in the dispersion parameter of a poisson model
# 3. thus poisson is nested within the negative binomial model
# 4. use a likelihood ratio test to compare nb:poisson and test this model assumption

# Poisson Model - Assumption Check ------------------------
summary(pm01 <- glm(count ~ date, data = jc, family = "poisson"))

#      Estimate   Std. Error  z value          Pr(>|z|)    
#   (Intercept) -2.519120492  0.086076543  -29.27 <0.0000000000000002 ***
#   date         0.000696546  0.000005234  133.08 <0.0000000000000002 ***

#     Null deviance: 1037319  on 306  degrees of freedom
# Residual deviance: 1019161  on 305  degrees of freedom
# AIC: NA

summary(qm01 <- glm(count ~ date, data = jc, family = "quasipoisson"))
# (Dispersion parameter for quasipoisson family taken to be 3001.57)

logLik(nb01)
logLik(pm01)

# check log likelihood
X2 <- 2 * (logLik(nb01) - logLik(pm01))
X2
# 'log Lik.' 1016291 (df=3)

pchisq(X2, df = 1, lower.tail=FALSE)
# 'log Lik.' 0 (df=3)

# Confidence Intervals - nb01
(est <- cbind(Estimate = coef(nb01), confint(nb01)))
#                      Estimate          2.5 %      97.5 %
#     (Intercept) -6.4525807535 -19.5113499876 6.904301489
#     date         0.0009358194   0.0001232975 0.001730696

exp(est)
#                      Estimate          2.5 %      97.5 %
#    (Intercept) 0.001576448 0.000000003359916 996.552168
#    date        1.000936257 1.000123305138063   1.001732

newdata01 <- data.frame(count = mean(jc$count), 
                        date = seq(as.Date("2015-07-08"), 
                                   as.Date("2015-12-31"), 
                                   by = "day"))

newdata01$phat <- predict(nb01, newdata01, type = "response")
head(newdata01)


# Predict on New Simulated Data 02: Using mean and max observed ---------------
0.5 * mean(jc$count)
max(jc$count)
# for an interval of 3:
# (count = rep(seq(min(jc$count), max(jc$count), length.out = 59), 3)
# simulating new data with 1/2 mean (3783.324) to max (20445) of observed counts

newdata02 <- data.frame(
  count = rep(seq(mean(jc$count)/2, max(jc$count), length.out = 177)), 
  date = seq(as.Date("2015-07-08"), as.Date("2015-12-31"), by = "day"))

# predict with nb01 on newdata02 and bind to results to newdata02
newdata02 <- cbind(newdata02, predict(nb01, newdata02, type = "link", se.fit = T))
# exponentiate fitted values and compute Lower and Upper Limits for range of acceptance
newdata02 <- within(newdata02, {
  fitted.count <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(newdata02, aes(date, fitted.count)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) +
  geom_line(aes(colour = fitted.count), size = 2) 


# Random Simulation: NB Dist 01 -----------------------------------------------
# Predict on randomly simulated values of Negative Binomial 
# from fitted nb01 model

restOfYear <- seq(as.Date("2015-07-08"), as.Date("2015-12-31"), by = "day")
random177 <- sample(nrow(jc), 177)

sim01 <- rnegbin(fitted(nb01)[random177], theta = k)
summary(sim01)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     364    3756    6259    7375   10180   24130
summary(jc$count)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      7    3559    7416    7567   10960   20440 

newdata03 <- data.frame(sim.count = sim01, date = restOfYear)
newdata03 <- cbind(newdata03, predict(nb01, newdata03, type = "link", se.fit = T))
newdata03 <- within(newdata03, {
  fitted.count <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

par(mfrow = c(2, 1), mar = c(4, 4, 4, 4))
plot(newdata03$date, newdata03$sim.count, main = "sim01 - Randomly simulated counts (July 2015-Dec 2015)",
     xlab = "date", ylab = "simulated count")
plot(jc$date, jc$count, main = "Observed counts by date (Jan 2014 - July 2015)",
     xlab = "date", ylab = "count")

# Random Simulation: NB Dist 02 -----------------------------------------------
# Predict on Sampled Values with Replacement

to2017 <- seq(as.Date("2015-07-08"), as.Date("2016-12-31"), by = "day")
random543 <- sample(jc$count, 543, replace = T)

sim02a <- data.frame(date = to2017, count = random543)
newdata04a <- data.frame(sim.count = sim02a, date = to2017)
newdata04a <- cbind(newdata04a, predict(nb01, newdata04a, type = "link", se.fit = T))
newdata04a <- within(newdata04a, {
  fitted.count <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

# Predict with Linear Model
new.dataLM <- data.frame(sim.count = random543, date = to2017)
new.dataLM <- cbind(new.dataLM, predict(lm01, new.dataLM, se.fit = T))
new.dataLM <- within(new.dataLM, {
  LL <- fit - 1.96 * se.fit
  UL <- fit + 1.96 * se.fit
})


sim02b <- rnegbin(fitted(nb01)[random177], theta = k)
newdata04b <- data.frame(sim.count = sim02b, date = restOfYear)
newdata04b <- cbind(newdata04b, predict(nb01, newdata04b, type = "link", se.fit = T))
newdata04b <- within(newdata04b, {
  fitted.count <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

par(mfrow = c(2, 2), mar = c(4, 4, 4, 4))
plot(newdata03$date, newdata03$sim.count, 
     main = "sim01 - Randomly simulated counts (July 2015-Dec 2015)\nNegatve Binomial distribution",
     xlab = "date", ylab = "simulated count")
plot(to2017, random543, main = "sim02a - sampled counts w/ replacement - July 2015-Dec 2016",
     xlab = "date", ylab = "simulated count")
plot(newdata04b$date, newdata04b$sim.count, 
     main = "sim02b - Randomly simulated counts, July 2015-Dec 2015\nNegatve Binomial distribution",
     xlab = "date", ylab = "count")
plot(jc$date, jc$count, main = "Observed counts by date (Jan 2014 - July 2015)",
     xlab = "date", ylab = "count")

# Plot Negative Binomial 01 ---------------------------------------------------

nb01p <- ggplot(jc, aes(date, count)) + 
  geom_line(aes(date, fit.lm01), color = "gold2", linetype = "dashed", size = 0.75) +
  geom_point(aes(date, count), color = "red3") +
  geom_point(aes(date, fit.nb01), color = "deepskyblue4", size = 2, alpha = 0.5) +
  scale_x_date(breaks = dates, labels = dates) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12.75),
        axis.title.y = element_text(margin = margin(0, 30, 0, 0),
                                    family = "Times New Roman",
                                    face = "italic")) +
  labs(title = "observed & negative binomial fitted values (count ~ date)", 
       x = "", y = "number of product listings (count)")

plot(nb01)
hist(jc$fit.nb01, breaks = 30)
hist(nb01a$.fitted)

# Do these points cluster in any significant way?

# new Date Scales 2014-2015
twoyrDaily <- seq(as.Date("2014-01-01"), as.Date("2015-12-31"), by = "day")
twoyrWeekly <- seq(as.Date("2014-01-01"), as.Date("2015-12-31"), by = "week")
twoyrMonthly  <- seq(as.Date("2014-01-01"), as.Date("2015-12-31"), by = "month")
threeYrMonthly <- seq(as.Date("2014-01-01"), as.Date("2016-12-31"), by = "month")

# plot with newdata02
nb01p2 <- ggplot(jc, aes(date, count)) + 
  geom_line(aes(date, fit.lm01), color = "gold2", linetype = "dashed", size = 0.75) +
  geom_point(aes(date, count), color = "red3") +
  geom_point(aes(date, fit.nb01), color = "deepskyblue4", size = 2, alpha = 0.5) +
  
  geom_point(data = newdata02, aes(date, count), color = "deeppink3", alpha = 0.75) +
  geom_point(data = newdata02, aes(date, fitted.count), color = "lightblue4", alpha = 0.5) +
  geom_ribbon(data = newdata02, aes(ymin = LL, ymax = UL), fill = "deepskyblue3", alpha = 0.25) +
  
  scale_x_date(breaks = twoyrMonthly , labels = twoyrMonthly) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12.75),
        axis.title.y = element_text(margin = margin(0, 30, 0, 0),
                                    family = "Times New Roman",
                                    face = "italic")) +
  labs(title = "observed & negative binomial fitted values (count ~ date)", 
       x = "", y = "number of product listings (count)")


# plot with newdata03 - simulated randomly
nb01sim01 <- ggplot(newdata03, aes(date, sim.count)) + 
  geom_line(data = jc, aes(date, fit.lm01), color = "gold2", linetype = "dashed", size = 0.75) +
  geom_point(data = jc, aes(date, count), color = "firebrick3", size = 2) +
  geom_point(data = jc, aes(date, fit.nb01), color = "deepskyblue4", size = 2, alpha = 0.5) +
  
  geom_point(data = newdata03, aes(date, sim.count), 
             color = "deeppink3", alpha = 0.75) +
  geom_point(data = newdata03, aes(date, fitted.count), 
             color = "lightblue4", alpha = 0.5) +
  geom_ribbon(data = newdata03, aes(ymin = LL, ymax = UL), 
              fill = "lightblue2", color = "deepskyblue3", alpha = 0.25) +
  
  scale_x_date(breaks = twoyrMonthly , labels = twoyrMonthly) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12.75),
        axis.title.y = element_text(margin = margin(0, 30, 0, 0),
                                    family = "Times New Roman",
                                    face = "italic")) +
  labs(title = "observed & negative binomial fitted values (count ~ date)\n
       + predicted values on simulated data", 
       x = "", y = "number of product listings (count)")

# plot with newdata04b - simulated randomly 
nb01sim02b <- ggplot(newdata04a, aes(date, sim.count)) + 
  geom_line(data = jc, aes(date, fit.lm01), color = "gold2", linetype = "dashed", size = 0.75) +
  geom_point(data = jc, aes(date, count), color = "firebrick3", size = 2) +
  geom_point(data = jc, aes(date, fit.nb01), color = "deepskyblue4", size = 2, alpha = 0.5) +
  
  geom_point(data = newdata04b, aes(date, sim.count), 
             color = "deeppink2", alpha = 0.75) +
  geom_point(data = newdata04b, aes(date, fitted.count), 
             color = "lightblue4", alpha = 0.5) +
  geom_ribbon(data = newdata04b, aes(ymin = LL, ymax = UL), 
              fill = "lightblue2", alpha = 0.35) +
  
  scale_x_date(breaks = twoyrMonthly , labels = twoyrMonthly) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12.75),
        axis.title.y = element_text(margin = margin(0, 30, 0, 0),
                                    family = "Times New Roman",
                                    face = "italic")) +
  labs(title = "Count ~ Date: observed, glm.nb fit, and predicted (sim02b) values", 
       x = "", y = "number of product listings (count)")

# plot with newdata04a - sampled w/ replacement
nb01sim02a <- ggplot(newdata04a, aes(date, sim.count)) + 
  geom_line(data = jc, aes(date, fit.lm01), color = "gold2", 
            linetype = "dashed", size = 0.75) +
  geom_line(data = new.dataLM, aes(date, fit), color = "gold3", 
            linetype = "dotdash", size = 0.90) +
  
  geom_point(data = jc, aes(date, count), color = "firebrick3", size = 2) +
  geom_point(data = jc, aes(date, fit.nb01), 
             color = "deepskyblue4", size = 2, alpha = 0.5) +
  
  geom_point(data = newdata04a, aes(date, sim.count), 
             color = "firebrick4", shape = 1, size = 2) +
  geom_ribbon(data = newdata04a, aes(ymin = LL, ymax = UL), 
              fill = "lightblue2", alpha = 0.25) +
  geom_point(data = newdata04a, aes(date, fitted.count),
             color = "deepskyblue4", shape = 1, alpha = 0.35) + 
  
  scale_x_date(breaks = threeYrMonthly, labels = threeYrMonthly) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12.75),
        axis.title.y = element_text(margin = margin(0, 30, 0, 0),
                                    family = "Times New Roman",
                                    face = "italic")) +
  labs(
    title = "Count ~ Date: observed, glm.nb fit, and predicted (sample02b) values", 
    x = "", y = "number of product listings (count)")


# Predict Until 2020 ----------------------------------------------------------

# extreme extrapolation

to2024 <- seq(as.Date("2015-07-08"), as.Date("2023-12-31"), by = "day")
set.seed(144)
random3099 <- sample(jc$count, 3099, replace = T)

par(mfrow = c(1, 1))
plot(to2024, random3099)

sim2024 <- data.frame(sim.count = random3099, date = to2024)
sim2024 <- cbind(sim2024, predict(nb01, sim2024, type = "link", se.fit = T))
sim2024 <- within(sim2024, {
  fitted.count <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

lm2024 <- data.frame(lm.count = random3099, date = to2024)
lm2024 <- cbind(lm2024, predict(lm01, lm2024, se.fit = T))


to2024Monthly <- seq(as.Date("2014-01-01"), as.Date("2023-12-31"), by = "month")
to2024Yr <- seq(as.Date("2014-01-01"), as.Date("2023-12-31"), by = "year")

nb2024 <- ggplot(sim2024, aes(date, sim.count)) + 
  geom_line(data = jc, aes(date, fit.lm01), color = "gold2", 
            linetype = "dashed", size = 0.75) +
  geom_line(data = lm2024, aes(date, fit), color = "gold3", 
            linetype = "dotdash", size = 0.90) +
  
  geom_point(data = jc, aes(date, count), color = "firebrick3", size = 2) +
  geom_point(data = jc, aes(date, fit.nb01), 
             color = "deepskyblue4", size = 2, alpha = 0.5) +
  
  geom_point(data = sim2024, aes(date, sim.count), 
             color = "firebrick4", shape = 1, size = 2) +
#  geom_ribbon(data = sim2024, aes(ymin = LL, ymax = UL), 
#              fill = "lightblue2", alpha = 0.25) +
  geom_point(data = sim2024, aes(date, fitted.count),
             color = "deepskyblue4", shape = 1, alpha = 0.35) + 
  
  scale_x_date(breaks = to2024Yr, labels = to2024Yr) +
  
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12.75),
        axis.title.y = element_text(margin = margin(0, 30, 0, 0),
                                    family = "Times New Roman",
                                    face = "italic")) +
  labs(
    title = "Count ~ Date: observed, glm.nb fit, and predicted (sim2024) values", 
    x = "", y = "number of product listings (count)")



