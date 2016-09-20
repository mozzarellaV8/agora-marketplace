# gist by runn1ng
# https://gist.github.com/runn1ng/23871c50784288b31522
# requires object 'black' 

library(ggplot2)
library(gtable)
library(grid)

png(file="~/marketlifetimes.png", width = 1000, height = 850)

grid.newpage()


#right-aligned
p1 <- ggplot(black, aes(colour=Closure)) +
  geom_segment(aes(x=Start, xend=End, y=reorder(Market,Start), yend=reorder(Market,Start)), size=3) +
  xlab("Open") + ylab("Market") + theme(legend.position="bottom", axis.text.y=element_text(hjust=1))

#left-aligned
p2 <- ggplot(black, aes(colour=Closure)) +
  geom_segment(aes(x=Start, xend=End, y=reorder(Market,Start), yend=reorder(Market,Start)), size=3) +
  xlab("Open") + ylab("Market") + theme(legend.position="bottom", axis.text.y=element_text(hjust=0))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))


pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                     pp$l, pp$b, pp$l)


# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ax <- g2$grobs[[ia]]$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
##############################
grid.draw(g)



invisible(dev.off())