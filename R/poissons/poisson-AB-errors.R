# broom example ---------------------------------------------------------------

# not so tidy results binding,
# because of different datasets
pre00 <- tidy(preSR2.00, conf.int = T)
pre00$id <- "00"
pre01 <- tidy(preSR2.01, conf.int = T)
pre01$id <- "01"
post01 <- tidy(post.sr2.q01, conf.int = T)
post01$id <- "ps01"
post02 <- tidy(pSR.q01, conf.int = T)
post02$id <- "ps02"
junes.b <- tidy(junes01, conf.int = T)
junes.b$id <- "junes"

weekly.regressions <- rbind(pre00, pre01, post01, post02, junes.b)


# tidyverse coefficients
coefs <- weekly.regressions %>% ungroup() %>% 
  filter(term == "week")
mutate(id = reorder(id, estimate))

ggplot(coefs, aes(estimate, id)) + geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, color = "red3")

# .std.resids -----------------------------------------------------------------
std.resid00 <- junes00 %>% ungroup() %>%
  mutate(id = reorder(id, .std.resid))

ggplot(std.resid00, aes(id, exp(.std.resid))) + geom_point() +
  geom_vline(xintercept = 0, color = "red3")

ggplot(std.resid00, aes(id, exp(.fitted))) + 
  geom_point(shape = 1, color = "deepskyblue4", size = 3) +
  geom_point(aes(id, count), shape = 19, size = 1, color = "firebrick3") +
  geom_hline(yintercept = 0, color = "red3")

# Fitted Values ---------------------------------------------------------------
pop01 <- augment(pop00)
pre00 <- wf00
pre01 <- augment(preSR2.01)
post01 <- augment(post.sr2.q01)
post02 <- augment(pSR.q01)

pop01$id <- "population"
pre00$id <- "pre00"
pre01$id <- "pre01"
post01$id <- "post01"
post02$id <- "post02"
jf$id <- "Junes"

prePostJunes <- rbind(pre00, pre01, post01, post02, jf, pop01)

ppj <- prePostJunes %>% ungroup() %>%
  mutate(id = reorder(id, exp(.resid), decreasing = T))

# Plot Fitted Values ----------------------------------------------------------
ppj.e <- ggplot(ppj, aes(id, exp(.fitted))) + 
  geom_point(shape = 1, color = "deepskyblue4", size = 6, alpha = 0.75) +
  geom_point(aes(id, count), shape = 19, size = 2, color = "firebrick3", alpha = 0.55) +
  geom_hline(yintercept = 0, color = "red3") +
  
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(margin = margin(0, 0, 30, 0)),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(family = "Times New Roman", face = "italic",
                                       size = 13.8, margin = margin(30, 0, 0, 0)),
        axis.title.y = element_text(family = "Times New Roman", face = "italic",
                                    size = 13.8, margin = margin(0, 30, 0, 0)))
  
ppj.e + labs(title = "Fitted vs Observed Values - (6) Quasipoisson Models",
       x = "quasipoisson fits before, after, and including SR2 shut down interval",
       y = "fitted vs. observed values")



