# Agora Marketplace
# product variable cleanse + subset

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(qdap)
library(data.table)
library(ggplot2)

p14 <- fread("~/GitHub/agora-data/ag06-2014.csv", stringsAsFactors = T)
str(p14)

# write.csv(p14, file = "~/GitHub/agora-data/ag05-2014.csv", row.names = F)

# Subset by Time Interval -----------------------------------------------------

length(unique(p14$date)) # 139

p14$listdate <- p14$date
p14 <- separate(p14, listdate, into = c("year", "month", "day"), sep = "-")

mDist <- as.data.frame(table(p14$month))
colnames(mDist) <- c("month", "numListings")
dDist <- as.data.frame(table(p14$day))
colnames(dDist) <- c("day", "numListings")

par(mar = c(5, 5, 5, 5), mfrow = c(1, 2), bty = "l", family = "HersheySans")
plot(mDist$month, mDist$numListings, 
     main = "number of listings by month - Agora 2014")
plot(dDist$day, dDist$numListings, 
     main = "number of listings by day - Agora 2014")

# p14 <- as.data.frame(p14)
# mDist$month <- as.character(mDist$month)
# p14$monthlyTotal <- left_join(p14, mDist, by = "month")

# append with feedback variable -----------------------------------------------

# p14$fb <- factor(p14$feedback) # 450084 levels
# length(unique(p14$feedback))   # 450084

# manually:
p14$great <- grepl("^\\sFeedbacks: 5/5(.*)", p14$feedback)
p14$good <- grepl("^\\sFeedbacks: 4/5(.*)", p14$feedback)
p14$ok <- grepl("^\\sFeedbacks: 3/5(.*)", p14$feedback)
p14$poor <- grepl("^\\sFeedbacks: 2/5(.*)", p14$feedback)
p14$horrible <- grepl("^\\sFeedbacks: 1/5(.*)", p14$feedback)
p14$worst <- grepl("^\\sFeedbacks: 0/5(.*)", p14$feedback)
p14$none <- grepl("^\\sFeedbacks: No feedbacks found.(.*)", p14$feedback)

# subset only feedback ratings ------------------------------------------------
feedback <- subset(p14, select = c(list, vendor, great, good, ok, poor,
                                   horrible, worst, none))

length(unique(feedback$vendor)) # 2284
fbUnique <- subset(feedback, select = c(list, unique(feedback$vendor), great, good,
                                        ok, poor, horrible, worst, none))

length(p14$great[p14$great == TRUE]) 
length(p14$great[p14$great == FALSE]) 
445550/1018109  # 0.437625
572559/1018109  # 0.562375

length(p14$good[p14$good == TRUE]) 
length(p14$good[p14$good == FALSE]) 
6170/1018109    # 0.01320913
1011939/1018109 # 0.9939397

length(p14$ok[p14$ok == TRUE]) 
length(p14$ok[p14$ok == FALSE]) 
3720/1018109    # 0.003653833
1014389/1018109 # 0.9963462

length(p14$poor[p14$poor == TRUE]) 
length(p14$poor[p14$poor == FALSE]) 
1456/1018109    # 0.001430102
1016653/1018109 # 0.9985699

length(p14$horrible[p14$horrible == TRUE])
length(p14$horrible[p14$horrible == FALSE])
1799/1018109    # 0.001767001
1016310/1018109 # 0.998233

length(p14$worst[p14$worst == TRUE])
length(p14$worst[p14$worst == FALSE])
8605/1018109    # 0.008451944
1009504/1018109 # 0.9915481

length(p14$none[p14$none == TRUE])
length(p14$none[p14$none == FALSE])
550802/1018109  # 0.5410049
467307/1018109  # 0.4589951


fDist <- data.frame(fb = c("great", "good", "ok", "poor", "horrible", "worst", "none"), 
                    true = c(445550, 6170, 3720, 1456, 1799, 8605, 550802), 
                    false = c(572559, 1011939, 1014389, 1016653, 1016310, 1009504, 467307))

par(mfrow = c(1, 1))
plot(fDist$fb, fDist$true)

totalListings <- 1018109
# fDist$pctT <- lapply(fDist$true, function(x) {fDist$true[[x]]/totalListings})
fDist <- transform(fDist, pct.T = true/totalListings)
fDist <- transform(fDist, pct.F = false/totalListings)

# write.csv(feedback, file = "~/GitHub/agora-data/feedback-matrix.csv", row.names = F)
# write.csv(fDist, file = "~/GitHub/agora-data/feedback-table.csv", row.names = F)

# plot feedback densities -----------------------------------------------------
feedback$list <- NULL
feedback$vendor <- NULL

fbs <- stack(feedback)
fbs$values[fbs$values == FALSE] <- 0
fbs$values[fbs$values == TRUE] <- 1
colnames(fbs) <- c("values", "feedback")

fbh1 <- ggplot(fbs, aes(values)) + 
  geom_histogram(binwidth = 50)
fbh1


fbsp <- ggplot(fbs, aes(values)) + xlim(0.5, 1) + 
  geom_density(aes(colour = feedback), alpha = 0.35) +
  scale_colour_discrete(limits = c("great", "good", "ok", "poor", "horrible",
                                 "worst", "none")) +
  theme_minimal(base_size = 18, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(size = 16)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Distributions of Feedback Values", x = "", y = "")

fbsp

fbsp2 <- ggplot(fbs, aes(values)) + xlim(0.5, 1) + 
  geom_density(aes(fill = feedback), alpha = 0.15) +
  scale_fill_discrete(limits = c("great", "good", "ok", "poor", "horrible",
                                   "worst", "none")) +
  theme_minimal(base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Distributions of Feedback")

fbsp2

fbv <-  ggplot(fbs, aes(feedback, values)) +
  geom_violin() + 
  theme_minimal(base_size = 18, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(size = 20)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Distributions of Feedback Values", x = "", y = "")

fbv

# aggregate feedbacks into tighter groups -------------------------------------

feedback$positive <- ifelse(feedback$great == T | feedback$good == T, 1, 0)
feedback$neutral <- ifelse(feedback$ok == T | feedback$poor == T, 1, 0)
feedback$negative <- ifelse(feedback$horrible == T | feedback$worst == T, 1, 0)

fb <- subset(feedback, select = c("positive", "neutral", "negative", "none"))
fb$none <- ifelse(fb$none == T, 1, 0)

fb2 <- stack(fb)
colnames(fb2) <- c("value", "feedback")

# violin
fbv2 <- ggplot(fb2, aes(feedback, value)) +
  geom_violin() + 
  theme_minimal(base_size = 18, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(size = 20)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Distributions of Feedback Values (aggregated)", 
       x = "", y = "")

fbv2

# lines
fbd <- fbsp <- ggplot(fb2, aes(value)) + xlim(0.5, 1) + 
  geom_density(aes(colour = feedback)) +
  scale_colour_discrete(limits = c("positive", "neutral", "negative", "none")) +
  theme_minimal(base_size = 18, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(size = 20)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Distributions of Feedback Values (aggregated)",
       x = "", y = "")

fbd

# filled
fbd2 <- fbsp <- ggplot(fb2, aes(value)) + xlim(0.5, 1) + 
  geom_density(aes(fill = feedback), alpha = 0.35) +
  scale_fill_discrete(limits = c("positive", "neutral", "negative", "none")) +
  theme_minimal(base_size = 18, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Distributions of Feedback Values (aggregated)")

fbd2






