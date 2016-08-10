# Agora Marketplace Analysis
# scrape - vendor text
# harvesting text data from vendor pages

library(rvest)
library(magrittr)
library(tm)
library(XML)
library(tidyr)

# vendor directory - single page ----------------------------------------------

# set directory
vendDir <- "~/GitHub/agora/2014-01-01/vendor/PurityStandard.html"

# parse html
vendor <- read_html(vendDir)

# vendor name ---------------------------------------------
vendor_name <- vendor %>%
  html_nodes("strong") %>%
  html_text()
vendor_name
# [1] "PurityStandard" "5/5"
# gave the rating too; can be separated later.

# vendor rating -------------------------------------------
vendorRating <- vendor %>%
  html_nodes(".gen-user-ratings") %>%
  html_text(trim = T)
vendorRating
# [1] " ~5/5, 1~2 deals" "[0 deals]" 

# vendor bio ----------------------------------------------
vendorBio <- vendor %>%
  html_nodes(".vendorbio-description") %>%
  html_text(trim = T)
vendorBio
# [1] "Welcome to my vendor page! Vatican is now PurityStandard here!

# pgp public key ------------------------------------------
vendorPGP <- vendor %>%
  html_nodes(".pgptoken") %>%
  html_text(trim = T)
vendorPGP
# [1] "-----BEGIN PGP PUBLIC KEY BLOCK-----Version: ...

# feedback -------------------------------------------------
vendorFeedback <- vendor %>%
  html_nodes(".embedded-feedback-list") %>%
  html_text(trim = T)

vendorFeedback <- stripWhitespace(vendorFeedback)
vendorFeedback
# [1] "Feedbacks: 5/5 thank you again purityStandard! 
# u guys are the best and most professional vendor on any marketplace :) 
# 500mg DMT Freebase 0 days ago anon [0 deals]"






