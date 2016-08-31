# Agora Marketplace Analysis
# Placeholder listings

# Often on Agora there will be products listed with exoribitant prices.
# While on the surface they may resemble scams, it's been observed (cit. Econ, other paper)
# that these prices are here for vendors to keep their listings active while
# waiting for their supply to be restocked. The prices are set high to 
# discourage transactions, but keep their listings active to 'advertise'
# for the near-future and maintain their market presence.

# In some ways, this can be seen as a 'waitlist' for certain products.
# As opposed to buying a rare or one-of-a-kind item, to maintain a 
# placeholder listing suggests at the very least a perceived demand
# for the product listed and renewable supply. 

# This pursuit arose from trying to determine a price cutoff for 
# what is a placeholder and what is an actual listing - the area becomes
# grey at the price points between $1000 and xxTBD. At the lower bound 
# of 1000 USD there can be legitimate bulk listings found for MDMA, 
# usually pressed pills in quantities of 50, 100, or 250. 

# What becomes problematic is another item such as a kilogram of cocaine
# has a legitimate street value of $40,000 - and there are listings for such.
# within that range, many products of lesser street value can be exist as
# placeholers, and thus live within the bounds of two legitimate listings.

# So - a strategy of single price cutoff point - while convenient - will ultimately
# rule out legitimate listings or include spurious ones. 

# data loaded from EDA-03.R ---------------------------------------------------

# p14 <- fread("~/GitHub/agora-data/ag03-2014.csv", stringsAsFactors = F)

# subset over $1000 -----------------------------------------------------------

# the highest bound on price
waitForever <- subset(p14, p14$price >= 99999.000)
write.csv(waitForever, file = "~/GitHub/agora-data/waitForever.csv", row.names = F)

# this zone is mostly populated with listings saying explicitly
# DONT ORDER. some have feedback and could be considered placeholders; 
# but most are annoying.

# remove optiman? he uses high prices to solicit collaborators;
# the listing is 'have something to sell? got an idea?' 
optiman <- waitlist[waitlist$vendor == "optiman", ]


# Placeholders and Outliers ---------------------------------------------------
waitlist <- subset(p14, p14$usd > 1200) # 96458
waitlist <- waitlist[order(waitlist$price, decreasing = T), ]


# General counts of categories ----------------------------

library(ggplot2)
# waitlist subcategory
wt <- as.data.frame(table(waitlist$subcat))
wt <- wt[order(wt$Freq, decreasing = T), ]
colnames(wt) <- c("subCategory", "Freq")
rownames(wt) <- NULL

wt <- wt[-c(36:46), ]
wt <- na.omit(wt)

# there's 34 obs - very likely with real and false listings.
# but now we have of narrowing this down a bit. 

wt2 <- as.data.frame(table(waitlist$subsubcat))
wt2 <- wt2[order(wt2$Freq, decreasing = T), ]
wt2 <- na.omit(wt2)
colnames(wt2) <- c("subSubCategory", "Freq")
rownames(wt2) <- NULL
# remove 0 frequencies
wt2 <- wt2[-c(41:42), ]

# plot a few to see
wtP1 <- ggplot(wt, aes(subCategory, Freq, fill = Freq)) + 
  geom_point() + 
  labs(title = "Agora 2014: Category / Subcategory", 
       y = "", x = expression(subcat %subset% cat)) +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(-20, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 9))

wtP1

stem(wt$Freq)
stem(wt2$Freq, scale = 2)

plot(ppois(0:10000,4))



