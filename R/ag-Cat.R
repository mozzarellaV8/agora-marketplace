# Agora Marketplace Analysis
# create directories for 'cat' pages
# with folder names that match crawl dates

# function test
dir.create(path = "~/GitHub/ag-Cat/2014-01-09")

# use the date column for naming vector
pv <- read.csv("data/crawl-distribution.csv")

# set folder names and parent directory
folders <- as.character(pv$date)
parent.folder <-"~/GitHub/ag-Cat/"

# loop to write directories
for (i in 1:length(folders)) {
  dir.create(paste(parent.folder, folders[i], sep = "/"))
}