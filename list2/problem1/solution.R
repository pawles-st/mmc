rand.unif <- runif(10000)

# exp

for (lambda in c(1, 1.5, 5)) {
	jpeg(paste0("exp_", lambda, ".jpeg"))
	rand.exp <- -log(rand.unif) / lambda
	hist(rand.exp, prob = TRUE, breaks = 30)
	pts <- seq(0, 10, by = 0.01)
	exp.density <- dexp(pts, rate = lambda)
	lines(pts, exp.density, col = 2, lwd = 3)
	dev.off()
}

# Weibull

for (lambda in c(1, 1.5, 5)) {
	for (k in c(0.5, 2, 5)) {
		jpeg(paste0("weibull_", lambda, "_", k, ".jpeg"))
		rand.weib <- lambda * (-log(rand.unif))^(1 / k)
		hist(rand.weib, prob = TRUE, breaks = 30)
		pts <- seq(0, 100, by = 0.01)
		weib.density <- dweibull(pts, scale = lambda, shape = k)
		lines(pts, weib.density, col = 2, lwd = 3)
		dev.off()
	}
}

# Cauchy

jpeg("cauchy.jpeg")
rand.cauchy <- tan(pi * (rand.unif - 0.5))
rand.cauchy <- rand.cauchy[abs(rand.cauchy) < 20]
hist(rand.cauchy, prob = TRUE, breaks = 100)
pts <- seq(-20, 20, by = 0.01)
cauchy.density <- dcauchy(pts)
lines(pts, cauchy.density, col = 2, lwd = 3)
dev.off()

help("dcauchy")
