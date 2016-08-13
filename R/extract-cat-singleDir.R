# Agora Marketplace Analysis
# extracting data from a category directory

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)


mainCatDir <- "~/GitHub/ag-Cat/2014-01-01"
setwd(mainCatDir)

catDir <- list.files(path = mainCatDir, pattern = ".html", all.files = T)

# Date and Category Name ------------------------------------------------------

# create blank frame
category <- data.frame(stringsAsFactors = F)

# extract main category
for (i in 1:length(catDir)) {
  log <- read_html(catDir[i])
  cat <- log %>%
    html_nodes("title") %>%
    html_text()
  category[i, 1] <- cat
}

category$date <- "2014-01-01"
colnames(category) <- c("cat01", "date")
category <- category[c(2, 1)]

summary(as.factor(category$cat01))
catTab <- as.data.frame(table(category))
catTab <- catTab[order(catTab$Freq, decreasing = T), ]

write.table(catTab, file = "~/GitHub/agora-data/AgScrape/catTab.csv",
            sep = ",", row.names = F)
