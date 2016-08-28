# Agora Marketplace Analysis
# subfolder calls.
# http://stackoverflow.com/questions/29122723/read-multiple-text-files-from-multiple-folders

parent.folder<-"~/GitHub/ag-Prouct"
setwd(parent.folder)

sub.folders1 <- list.dirs(parent.folder, recursive=TRUE)[-1]
sub.folders2 <- list.dirs(sub.folders1, recursive=FALSE)
r.scripts <- file.path(sub.folders2)

for (k in r.scripts){
  file.name.v <- list.files(k, pattern="*.html")
  for (f in file.name.v){
    file.read.v <- scan(paste(k, f, sep="/"),
                        what ="character",sep="\n")
  }
}


# x is a single html file in a directory

nameframe <- data.frame()

name <- function(x) {
  log <- read_html(x)
  log <- x %>% html_nodes("#single-product h1") %>%
    html_text()
  nameframe <- rbind(log, x)
}