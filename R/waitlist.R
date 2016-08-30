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


# Placeholders and Outliers
waitlist <- subset(p14, p14$price >= 3500)
waitlist <- waitlist[order(waitlist$price, decreasing = T), ]

waitTable <- as.data.frame(table(waitlist$subcat))
waitTable <- waitTable[order(waitTable$Freq, decreasing = T), ]
colnames(waitTable) <- c("subCategory", "Freq")
table(waitlist$cat)

# snapshotp of what's out of stock
waitT2 <- as.data.frame(table(waitlist$subsubcat))