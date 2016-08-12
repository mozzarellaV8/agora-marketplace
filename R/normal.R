# annotated normal distribution

## plot the function from -3 to 3
curve(dnorm, from = -3, to = 3, n = 1000, xlab = "x", ylab = expression(P(x)),
      main = "Normal Probability Density Function")
## add the formula to the plot
text(-2, 0.3, expression(P(x) == paste(frac(1, sqrt(2 * pi * sigma^2)),
                                       " ", e^{
                                         frac(-(x - mu)^2, 2 * sigma^2)
                                       })), cex = 1.2)
