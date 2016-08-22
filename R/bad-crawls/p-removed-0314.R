# Agora Marketplace Analysis
# 0314 p removed

tail(products0314)
pList[5531] # [1] "2014-02-02__yX33u1N9xx.html"
pList[5766] # [1] "2014-02-02__zVnbUd8n7U.html"
pList[5531] # [1] "2014-02-02__yX33u1N9xx.html"
pList[4932]


products0314$cat == "Data"
products0314[201, ]
pList[201]

levels(products0314$cat)
products0314$cat == "Forgeries"
pList[4708]



for (i in 1:length(pList)) {
  pLog2 <- read_html(pList[i])
  
  if (products0314$cat != "Electronics" && products0314$cat != "Listings" && products0314$cat != "Data") {
    
    pTab2 <- pLog %>%
      html_nodes(".topnav-element a") %>%
      extract2(2) %>%
      html_text()
    
    products0314$subcat <- rbind(products0314$subcat, pTab2)
  } else {
    
    pTab2 <- NA
    products0314$subcat <- rbind(products0314$subcat, pTab2)
    
  }
}
