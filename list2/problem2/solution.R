rand.unif1 <- runif(10000)
rand.unif2 <- runif(10000)

rand.norm1 <- sqrt(-2 * log(rand.unif2)) * cos(2 * pi * rand.unif1)
rand.norm2 <- sqrt(-2 * log(rand.unif2)) * sin(2 * pi * rand.unif1)

# normal distribution

jpeg("normal.jpeg")
hist(rand.norm1, prob = TRUE, breaks = 30)
pts <- seq(-10, 10, by = 0.01)
norm.density <- dnorm(pts)
lines(pts, norm.density, col = 2, lwd = 3)
dev.off()

# normal combination

rand.norm1 <- rand.norm1[1:1000]
rand.norm2 <- rand.norm2[1:1000]

jpeg("multi1.jpeg")
plot(rand.norm1, rand.norm2)
dev.off()

jpeg("multi2.jpeg")
plot(rand.norm1, rand.norm1 + rand.norm2)
dev.off()

jpeg("multi3.jpeg")
plot((rand.norm1 + rand.norm2) / sqrt(2), (rand.norm1 - rand.norm2) / sqrt(2))
dev.off()



