# inspect when loop stalls:
tail(products0114)

pList[31] # [1] "2014-01-01__B7N1C81BaX.html" // all forgeries
pList[39] # [1] "2014-01-01__BEUtSxVbaG.html"
pList[71] # [1] "2014-01-01__ClfKoiUqSg.html"
pList[86] # [1] "2014-01-01__d1068qcJjd.html"
pList[101] # [1] "2014-01-01__dqtUV15RuD.html"
pList[103] # [1] "2014-01-01__dqtUV15RuD.html"
pList[149] # [1] "2014-01-01__f4gY5C9UJD.html"
pList[271] # [1] "2014-01-01__jad8aEgmAw.html"
pList[286] # [1] "2014-01-01__JqRg6rCBaB.html" // Jewelry
# [1] "2014-01-01__Jria9AeMnW.html" // Forgeries
pList[342] # [1] "2014-01-01__Lb1bLyMcqw.html" // Jewelry
pList[479] # [1] "2014-01-01__NwsBgY0hTb.html" // forgeries
pList[485] # [1] "2014-01-01__o1snwBD2Br.html"
pList[579] # [1] "2014-01-01__r5atKhTz7F.html"
pList[585] # [1] "2014-01-01__R9F2gwlXUp.html"
pList[605] # [1] "2014-01-01__RpGvLN4L2h.html"
pList[689] # [1] "2014-01-01__TdsDo7nd9x.html"
pList[810] # [1] "2014-01-01__VhvRvqCdAw.html"
pList[874] # [1] "2014-01-01__wq8sTLdufg.html"
pList[959] # [1] "2014-01-01__yF9qjYYA92.html"
pList[980] # [1] "2014-01-01__yVpeT8YbSk.html"
pList[1083] # [1] "2014-01-09__AGs4ikK7bm.html"
pList[1084] # [1] "2014-01-09__ahS8TgtWZ1.html"
pList[1101] # [1] "2014-01-09__AV4Serqyms.html"
pList[1150] # [1] "2014-01-09__BMUEDcEp2l.html"
pList[1259] # [1] "2014-01-09__dTauxWKiNS.html"
pList[1312] # [1] "2014-01-09__EPnj2fgebU.html"
pList[1337] # [1] "2014-01-09__F3ZePuzzDS.html"

tail(products0114)
pList[2742] # [1] "2014-01-16__cCFuKaiJZd.html"
pList[2623] # [1] "2014-01-16__AV3a5aB85b.html"
pList[2561] # [1] "2014-01-16__a55fw80Wal.html"
pList[2480] # [1] "2014-01-09__z82NAvx43u.html"
pList[2227]
pList[2223] # [1] "2014-01-09__veh1gn7hRb.html"
pList[2055] # [1] "2014-01-09__sSCu33seoM.html"
pList[1941] # [1] "2014-01-09__qLgEzDvkxZ.html"
pList[1716] # [1] "2014-01-09__MaEPvJrnUq.html"
pList[1705] # [1] "2014-01-09__M1uRS11oUn.html"

pList[1091] # [1] "2014-01-01__DSzb9R1WUi.html"
pList[1226] # [1] "2014-01-09__Cq4y26dwGd.html" // listing page
pList[1298] # [1] "2014-01-09__dvKTDJyAkX.html" // listing page
pList[1837] # [1] "2014-01-09__ncpflLFXB7.html" // listing page
pList[1869] # [1] "2014-01-09__nuor15hThm.html" // listing page
pList[2004] # [1] "2014-01-09__r6f3lbgfyn.html"

write.csv(products0114, file = "products0114-sample.csv", row.names = F)





pList[10]
x <- pLog %>%
  html_nodes(".topnav-element a") %>%
  extract2(2) %>%
  html_text()

subcat <- function(x) {
  if(missing(x)) {
    next
  } else {
    x <- prod$subcat
  }
}