# Agra Marketplace Analysis
# Exploratory: Product & Feedback Variables

# load data -------------------------------------------------------------------

library(data.table)
library(tidytext)
library(qdap)
library(ggplot2)
library(RColorBrewer)

# population with: character counts, subcat agregated, all cats
a <- fread("~/GitHub/agora-data/agora-04.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables
str(a)

# create categories  ----------------------------------------------------------

# combine sub- and subsub-categories
a$sc <- paste(a$subcat, a$subsubcat, sep = "-")
a$sc <- gsub("-NA$", "", a$sc)
a$sc <- gsub("Methylone", "RCs", a$sc)
a$sc <- factor(a$sc)
levels(a$sc)

a$all.c <- paste(a$cat, a$sc, sep = ": ")
a$all.c <- gsub("Drugs: Methylone", "Drugs: RCs", a$all.c)
a$all.c <- factor(a$all.c)
levels(a$all.c)

# character counts
a$p.chars <- nchar(as.character(a$product))
a$f.chars <- nchar(as.character(a$feedback))
summary(a$f.chars)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0.0    30.0    30.0   283.4   262.0  2979.0

hist(a$p.chars, breaks = 200)
hist(log(a$p.chars), breaks = 100)

fbc <- subset(a, a$f.chars > 30)
nrow(fbc)
nchar("Feedbacks: No feedbacks found.")
hist(fbc$f.chars, breaks = 200)

# Word Counts -----------------------------------------------------------------
a$p.words <- word_count(a$product, byrow = T, digit.remove = F)
summary(a$p.words)
hist(a$p.words) # normal, wow
hist(log(a$p.words))

a$f.words <- word_count(a$feedback, byrow = T, digit.remove = F)
summary(a$f.words)
hist(fbc$f.words, breaks = 200)
summary(fbc$f.words)
fbc <- subset(fbc, fbc$f.words > 6)

# write.csv(a, file = "~/GitHub/agora-data/agora-02.csv", row.names = F)

a$fwd <- discretize(a$f.words, method = "interval", categories = 22)
a$fwd <- NULL

a$fwd <- ifelse(a$f.words <= 25, 25, 
                ifelse(a$f.words > 25 & a$f.words <= 50, 50, 
                       ifelse(a$f.words > 50 & a$f.words <= 75, 75,
                              ifelse(a$f.words > 75 & a$f.words <= 100, 100,
         ifelse(a$f.words > 100 & a$f.words <= 125, 125, 
                ifelse(a$f.words > 125 & a$f.words <= 150, 150,
                       ifelse(a$f.words > 150 & a$f.words <= 175, 175,
                              ifelse(a$f.words > 175 & a$f.words <= 200, 200,
         ifelse(a$f.words > 200 & a$f.words <= 225, 225, 
                ifelse(a$f.words > 225 & a$f.words <= 250, 250,
                       ifelse(a$f.words > 250 & a$f.words <= 275, 275,
                              ifelse(a$f.words > 275 & a$f.words <= 300, 300,
         ifelse(a$f.words > 300 & a$f.words <= 325, 325,
                ifelse(a$f.words > 325 & a$f.words <= 350, 350,
                       ifelse(a$f.words > 350 & a$f.words <= 375, 375,
                              ifelse(a$f.words > 375 & a$f.words <= 400, 400,
         ifelse(a$f.words > 400 & a$f.words <= 425, 425,
                ifelse(a$f.words > 425 & a$f.words <= 450, 450,
                       ifelse(a$f.words > 450 & a$f.words <= 475, 475,
                              ifelse(a$f.words > 475 & a$f.words <= 500, 500,
         ifelse(a$f.words > 500 & a$f.words <= 525, 525,
                ifelse(a$f.words > 525 & a$f.words <= 550, 550, NA))))))))))))))))))))))



# word count over time --------------------------------------------------------

# prep
dates <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "month")
dw <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "week")

summary(a$f.words)
hist(a$f.words)

# horizontal 
p.fw01 <- ggplot(a, aes(f.words, reorder(all.c))) + 
  geom_tile(aes(f.words, reorder(all.c), fill = f.words),  
            color = "white", na.rm = T) +
  scale_fill_gradient2(low = " deepskyblue4", mid = "bisque2",
                       high = "firebrick3", midpoint = 275) +
  scale_x_continuous(breaks = seq(0, 600, 25)) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  
  theme(
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"), 
    axis.text.y = element_text(size = 8, lineheight = 1.2, hjust = 1),
    axis.text.x = element_text(size = 12.75),
    axis.title.x = element_text(family = "Times New Roman", 
                                face = "italic", margin = margin(20, 0, 0, 0)),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.key = element_blank()) +
  
  labs(title = "Categories by Feedback (word count)",
       x = "number of words", y = "", fill = "")

p.fw01

# horizontal 2
p.fw02 <- ggplot(a, aes(fwd, reorder(all.c))) + 
  geom_tile(aes(fwd, reorder(all.c), fill = fwd),  
            color = "white", na.rm = T) +
  scale_fill_gradient2(low = " deepskyblue4", mid = "bisque2",
                       high = "firebrick3", midpoint = 270) +
  scale_x_continuous(breaks = seq(0, 600, 25)) +
  theme_minimal(base_size = 11, base_family = "GillSans") +
  
  theme(
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"), 
    axis.text.y = element_text(size = 8.25, lineheight = 1.2, hjust = 1),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(family = "Times New Roman", 
                                face = "italic", margin = margin(20, 0, 0, 0)),
    legend.text = element_text(size = 8)) +
  
  labs(title = "Categories by Feedback (~ number of words)",
       x = "number of words", y = "", fill = "")

p.fw02
