# Earthquake data - USGS
# Long Tail exploration

# load data -------------------------------------------------------------------

# two CSVs divided by year bc of USGS 20k limit on record downloads
eq14 <- read.csv("~/GitHub/agora-data/data-longtail/2014-EQ-R2.csv")
# 14353
eq15 <- read.csv("~/GitHub/agora-data/data-longtail/2015-EQ-R2.csv")
# 13532

# bind em
eq <- rbind(eq14, eq15)
summary(eq)
# time object is a mess

library(tidyr)

eq2 <- separate(eq, time, into = c("date", "time"), sep = "T")
eq2$date <- as.Date(eq2$date)
eq2$time <- gsub("0Z", "", eq2$time)
eq2$time <- separate(eq2, time, into = c("time", "milliseconds"), sep = ".")

summary(eq2)

# better table:
write.table(eq2, file = "~/GitHub/agora-data/data-longtail/agoraEarthquakes.csv",
            sep = ",", row.names = F)

# test it
eq <- read.csv("~/GitHub/agora-data/data-longtail/agoraEarthquakes.csv")
rm(eq2)

# explore ---------------------------------------------------------------------

hist(eq$mag, breaks = 20)

library(ggplot2)

magHist <- ggplot(eq, aes(x = mag)) +
  geom_histogram(stat = "bin", binwidth = 0.15)

magHist

magDepth <- ggplot(eq, aes(x = mag, y = depth)) +
  geom_point() 

magDepth + geom_smooth()






