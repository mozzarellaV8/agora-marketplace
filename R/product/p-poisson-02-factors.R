# downtime dates and Grams dataset
# compare these to the raw crawl counts 

# load ------------------------------------------------------------------------

library(data.table)
library(tidyr)

grams <- fread("~/GitHub/agora-data/00-archive/00.grams/data/agora.csv")
dt <- read.csv("data/agoraDNS.csv")

gramsDates <- as.data.frame(table(as.factor(grams$Date)))
colnames(gramsDates) <- c("date", "count")
gramsDates$date <- as.Date(gramsDates$date)
plot(gramsDates$date, gramsDates$count)

# write.csv(gramsDates, file = "data/GramsDates.csv", row.names = F)

# models ----------------------------------------------------------------------

# linear model
lmg <- lm(count ~ date, data = gramsDates)
summary(lmg)
# Coefficients:
#                 Estimate Std. Error t value            Pr(>|t|)    
# (Intercept)   -442774.10   38488.40   -11.5 <0.0000000000000002 ***
#   date             28.09       2.34    12.0 <0.0000000000000002 ***

# Residual standard error: 4076 on 226 degrees of freedom
# Multiple R-squared:  0.3893,	Adjusted R-squared:  0.3866 

# poisson model
pmg <- glm(count ~ date, family = "poisson", data = gramsDates)
summary(pmg)
# Null deviance: 348423  on 227  degrees of freedom
# Residual deviance: 222878  on 226  degrees of freedom

# plot
plot(gramsDates$date, pmg$fitted.values, ylim = c(0, 30000), pch = 2, cex = 1.1,
     main = "Count ~ Date, grams dataset")
points(gramsDates$date, gramsDates$count, col = "#CD262690", pch = 19)
points(gramsDates$date, lmg$fitted.values, col = "#FFE4C475", pch = 15)
points(gramsDates$date, pmg$fitted.values, col = "#00688B90", pch = 17)
axis(1, at = gramsDates$date, labels = gramsDates$date)

# get monthly sums ------------------------------------------------------------

gramsDates$fulldate <- gramsDates$date
gramsDates <- separate(gramsDates, date, into = c("year", "month", "date"), sep = "-")

# compare grams counts to crawl counts
july14 <- subset(gramsDates, gramsDates$month == "07" & gramsDates$year != "2015")
sum(july14$count) # 305894
mo$count[07]      # 39989
# crawl is off by factor of 10

dec14 <- subset(gramsDates, gramsDates$month == "12")
sum(dec14$count)  # 445531
mo$count[12]      # 259820

nrow(dec14) # 23
# two datasets: Grams(search engine API calls) and Crawls(qwerns' raw data)
# two perspectives:
# 01:
# althought the grams dataset reports for 23 unique days in December, 
# only 10 unique counts are collected - meaning that its simply repeating 
# one API call's values over a number of days.

# 02:
# another thought is - the crawls are definitely incomplete, and given the 
# apparent 'stability' of grams data, we might be able to reasonably impute 
# counts for missing/low outlier days in the crawl data. 
