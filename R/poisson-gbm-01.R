# Agora Marketplace Analysis
# gbm() - Poisson - Monthly, Weekly, Daily counts
# "How much is there? --> How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)

library(gbm)
library(rpart)

library(sandwich)
library(plsRglm)
library(caret)

library(ggplot2)
library(vcd)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)

wk <- fread("data/WeeklyCountsPop.csv")
wk$week <- as.Date(wk$week) 

mo <- fread("data/MonthlyAll.csv")
daily <- fread("data/")

# gradient boosted model 01 ---------------------------------------------------

# confusion matrix function
confusion <- function(a, b){
  tbl <- table(a, b)
  mis <- 1 - sum(diag(tbl))/sum(tbl)
  list(table = tbl, misclass.prob = mis)
}


# weekly counts
gbm01 <- gbm(count ~ as.factor(week), distribution = "poisson", data = wk)
gbm.perf(gbm01)
# 47

confusion(predict(gbm01, wk, n.trees = 47) > 0, wk$count > 0)


# daily counts
gbm.daily01 <- gbm(count~)






