rand.unif1 <- runif(10000)
rand.unif2 <- runif(10000)

rand.norm1 <- sqrt(-2 * log(rand.unif2)) * cos(2 * pi * rand.unif1)
rand.norm2 <- sqrt(-2 * log(rand.unif2)) * sin(2 * pi * rand.unif1)

# normal distribution

jpeg("normal.jpeg")
hist(rand.norm1, prob = TRUE, breaks = 30, main = "Verification of Box-Muller generation", xlab = "Value of normal distribution")
pts <- seq(-10, 10, by = 0.01)
norm.density <- dnorm(pts)
lines(pts, norm.density, col = 2, lwd = 3)
dev.off()

# normal combination

rand.norm1 <- rand.norm1[1:1000]
rand.norm2 <- rand.norm2[1:1000]

jpeg("multi1.jpeg")
plot(rand.norm1, rand.norm2, main = "Point plot for the vector (x1, x2)", xlab = "First axis", ylab = "Second axis")
dev.off()

jpeg("multi2.jpeg")
plot(rand.norm1, rand.norm1 + rand.norm2, main = "Point plot for the vector (x1, x1 + x2)", xlab = "First axis", ylab = "Second axis")
dev.off()

jpeg("multi3.jpeg")
plot((rand.norm1 + rand.norm2) / sqrt(2), (rand.norm1 - rand.norm2) / sqrt(2), main = "Point plot for the vector 1 / sqrt(2) * (x1 + x2, x1 - x2)", xlab = "First axis", ylab = "Second axis")
dev.off()



