# part 1

gen.mix <- function(p, m, s) {
	if (runif(1) < p) {
		return(rnorm(1, mean = m, sd = s))
	} else {
		return(rnorm(1, mean = 0, sd = 1))
	}
}

gen.mix.sample <- function(n, p, m, s) {
	mix.sample <- lapply(rep(p, sample.size), function(p) gen.mix(p, m, s))
	mix.sample <- unlist(mix.sample)
	return(mix.sample)
}

sample.size <- 5000
P <- c(0.5, 0.5, 0.2)
M <- c(5, 1, 10)
S <- c(2, 0.1, 1)
for (i in 1:3) {
	mix.sample <- gen.mix.sample(sample.size, P[i], M[i], S[i])
	x.values <- seq(min(mix.sample), max(mix.sample), by = 0.1)
	png(paste0("mix_sample_", i, ".png"))
	hist(mix.sample, prob = TRUE, breaks = "Freedman-Diaconis")
	lines(x.values, (1 - P[i]) * dnorm(x.values) + P[i] * dnorm(x.values, mean = M[i], sd = S[i]), type = "l", lwd = 3)
	dev.off()
}

# part 2

sample.size <- 500

gen.simplex.unif <- function() {
	while (TRUE) {
		coords <- runif(2)
		if (sum(coords) < 1) {
			return(coords)
		}
	}
}

simplex.unif.sample <- lapply(1:sample.size, function(x) gen.simplex.unif())
pts <- do.call(rbind, simplex.unif.sample)

png("simplex_unif.png")
plot(pts[, 1], pts[, 2])
dev.off()

gen.simplex.xy2 <- function() {
	while (TRUE) {
		p <- runif(2)
		x <- p[1] ^ (1/2)
		y <- p[2] ^ (1/3)
		if (x + y < 1) {
			return(c(x, y))
		}
	}
}

simplex.unif.sample <- lapply(1:sample.size, function(x) gen.simplex.xy2())
pts <- do.call(rbind, simplex.unif.sample)

png("simplex_xy2.png")
plot(pts[, 1], pts[, 2])
dev.off()

# part 3

gen.normal <- function(m, R) {
	return(m + R %*% rnorm(3))
}

sample.size <- 500

S <- matrix(c(2, 2, 0, 2, 5, -3, 0, -3, 9), nrow = 3)
R <- chol(S)
m = c(1, -1, 0)
normal.sample <- lapply(1:sample.size, function(x) gen.normal(m, R))
pts <- do.call(cbind, normal.sample)

png("norm1.png")
hist(pts[1, ], breaks = "Freedman-Diaconis")
dev.off()
png("norm2.png")
hist(pts[2, ], breaks = "Freedman-Diaconis")
dev.off()
png("norm3.png")
hist(pts[3, ], breaks = "Freedman-Diaconis")
dev.off()
png("norm12.png")
plot(pts[1, ], pts[2, ])
dev.off()
png("norm13.png")
plot(pts[1, ], pts[3, ])
dev.off()
png("norm23.png")
plot(pts[2, ], pts[3, ])
dev.off()
