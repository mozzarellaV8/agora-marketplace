# Association Rule Graphs
# loop to graph every rule

for (i in 1:100) {
  
  log <- head(sort(a2rules, by = c("support", "confidence", "lift")), i)
  
  png(filename = paste("~/GitHub/agora-local-market/igraphs/",i,".png"),
     width = 1800, height = 1200, pointsize = 18, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(144)
  
  plot(log, method = "graph", 
           main = paste(i, "rules ~ support + confidence + lift (dh)"),
           edge.color = "#00000025",
           vertex.frame.color="#00000025",
           vertex.color = pdpal(100),
           vertex.label.color = "grey8", 
           vertex.label.cex = 0.74, layout = layout_with_dh,
           vertex.label.dist = 0)
  
  dev.off()
  
}
