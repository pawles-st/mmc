rand.unif1 <- runif(10000)
rand.unif2 <- runif(10000)

rand.abs.norm <- sqrt(-2 * log(rand.unif2)) * abs(cos(2 * pi * rand.unif1))

# absolute normal

jpeg("abs_normal.jpeg")
hist(rand.abs.norm, prob = TRUE, breaks = 30, main = "Comparison of 'absolute normal' density\nwith its generation method", xlab = "Value of the distribution")
pts <- seq(0, 10, by = 0.01)
abs.norm.density <- 2 * dnorm(pts)
lines(pts, abs.norm.density, col = 2, lwd = 3)
dev.off()
