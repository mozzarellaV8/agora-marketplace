# Assosiations-04-mining-02
# a3: 2317353 observations of 4 variables:
# price, from (location), subcategory, vendor

a3 <- subset(ag, select = c("p", "f", "sc", "v"))
levels(a3$sc)

a3rules <- apriori(a3, parameter = list(support = 0.0025, confidence = 0.6,
                                        minlen = 2, maxlen = 5))

summary(a3rules)
arules::inspect(head(a3rules, 10))
arules::inspect(tail(a3rules, 10))

# Plot Rules - Group -----------------------------------------------------------

# individual
plot(a3rules, method = "grouped", control = list(k = 36))

# loop
for (i in 1:10) {
  
  png(filename = paste("~/GitHub/agora-local-market/arules/groups/g1-",i,".png"),
      width = 1800, height = 1400, pointsize = 20, bg = "transparent")
  
  k = i * 12
  
  plot(a3rules, method = "grouped", control = list(k = k), 
       main = paste("k =", k))
  
  dev.off()
  
}

# Plot Rules - Graph ----------------------------------------------------------

# get layouts
grep("^layout_", ls("package:igraph"), value = T)[-1]

# define a palette
pdpal <- colorRampPalette(c("#B2DFEE85", "#FFFFFF75", "#00688B85"), alpha = 0.85)

# plot by Support, Confidence, and Lift
r1 <- head(sort(a3rules, by = c("support", "confidence", "lift")), 212)
p1 <- plot(r1, method = "graph", 
           main = "212 rules ~ support + confidence + lift (dh)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.color = pdpal(100),
           vertex.label.color = "grey8", 
           vertex.label.cex = 0.68, layout = layout_with_dh,
           vertex.label.dist = 0)

hs <- hub_score(p1, weights=NA)$vector
as <- authority_score(p1, weights=NA)$vector
cliqueR1 <- sapply(cliques(p1), length)
largest_cliques(p1)


# plot by Lift
r2 <- head(sort(a3rules, by = "lift"), 36)
p2 <- plot(r2, method = "graph", 
           main = "36 rules ~  lift (kk)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.label.color = "grey2", 
           vertex.color = pdpal(100),
           vertex.label.cex = 0.68, layout = layout_with_kk,
           vertex.label.dist = 0)

# plot by Support and confidence
r3 <- head(sort(a3rules, by = c("support", "confidence")), 48)
p3 <- plot(r3, method = "graph", 
           main = "48 rules ~ support + confidence (kk)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.label.color = "grey2", 
           vertex.color = pdpal(100),
           vertex.label.cex = 0.68, layout = layout_with_kk,
           vertex.label.dist = 0)

# visNetwork ------------------------------------------------------------------
library(visNetwork)
p1df <- get.data.frame( p1, what = "both" )

visNetwork(
  nodes = data.frame(
    id = p1df$vertices$name
    ,value = p1df$vertices$lift # could change to lift or confidence
    ,title = ifelse(p1df$vertices$label == "",p1df$vertices$name, p1df$vertices$label)
    ,p1df$vertices
  )
  , edges = p1df$edges
)

# Plot Rules - Graph Loops ----------------------------------------------------

# r1: by Support, Confidence, and Lift ----------------------------------------

for (i in 1:212) {
  
  log <- head(sort(a3rules, by = c("support", "confidence", "lift")), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/r1-SCL-",i,".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  par(family = "GillSans")
  set.seed(144)
  plot(log, method = "graph", 
       main = paste(i, "rules ~ support + confidence + lift (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "grey8", 
       vertex.label.cex = 0.68, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}

# r2: by Lift ----------------------------------------------------------------

even <- seq(2, 100, 2)

for (i in 1:length(even)) {
  
  tmp <- head(sort(a2rules, by = "lift"), even[i])
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a4b-r2-Lift-",even[i],".png"),
      width = 1800, height = 1400, pointsize = 16, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(256)
  plot(tmp, method = "graph", 
       main = paste(even[i], "rules ~ lift (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "gray8", 
       vertex.label.cex = 0.70, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}

# r3: by Support and Confidence -----------------------------------------------

for (i in 48:144) {
  
  tmp <- head(sort(a2rules, by = c("support", "confidence")), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a4-r3-SC-",i,".png"),
      width = 1800, height = 1400, pointsize = 16, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(64)
  plot(tmp, method = "graph", 
       main = paste(i, "rules ~ support + confidence (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "grey8", 
       vertex.label.cex = 1, layout = layout_with_kk,
       vertex.label.dist = 0)
  
  dev.off()
}
