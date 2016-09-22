# logistic / linear


agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)

summary(agora$fb)
logistic01 <- glm(fb ~ cat, family = binomial(link = "logit"), data = agora)
summary(logistic01)
