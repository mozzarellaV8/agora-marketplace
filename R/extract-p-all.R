# here we go ------------------------------------------------------------------

library(data.table)

mainDir <- "~/GitHub/ag-Product"
pDir <- list.files(mainDir, pattern = ".html", all.files = T, 
                   recursive = T)

length(pDir)
# [1] 2,435,834
head(pDir)
# [1] "2014-01-01-p/a1BkWEipyk.html"
tail(pDir)
# [1] "2015-07-07-p/zZWKdcbhDq.html"

# pre-allocate / set dimentions
system.time(nameframe <- data.frame(date = 1:2435834))
#    user  system elapsed 
#   0.002   0.002   0.004 

# data.table test for pre-allocation
system.time(nameDT <- data.table(date = 1:2435834))
#    user  system elapsed 
#   0.005   0.005   0.010 

# this one cuts out the date and leaves the file name.
test <- gsub("^[^\\/]+", "", pDir[1])
# this one works - leaves just the date.
test2 <- sub(" *\\/.*", "", pDir[1])

setwd("~/GitHub/ag-Product")


# loop through product directory
# extract product name, write to dataframe
for (i in 1:length(pDir)) {
  log <- read_html(pDir[i])
  name <- log %>% 
    html_nodes("title") %>%
    html_text()
  nameframe$date <- sub(" *\\/.*", "", pDir[1])
  nameframe <- rbind(nameframe, name)
}

# for loop started at 9:00 PM
# killed the process at 10:25 PM
# saw in activiity monitor RStudio is only using 1.67 GB of real memory.
# pooped out at i = 9088L (name = "Brain Generator  Crack")
