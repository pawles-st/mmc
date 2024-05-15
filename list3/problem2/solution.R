# part 1

sample.size <- 1000

unif.sample <- runif(sample.size)
estimates <- c()
for (n in 1:sample.size) {
	n.estimate <- sum(exp(-unif.sample[1:n]^2)) / n
	estimates <- c(estimates, n.estimate)
}

norm.sample <- rnorm(sample.size, sd = sqrt(2) / 2)
estimates <- c()
variances <- c()
for (n in 1:sample.size) {
	n.sample <- norm.sample[1:n]
	n.estimate <- sqrt(pi) * length(n.sample[n.sample >= 0 & n.sample <= 1]) / n
	estimates <- c(estimates, n.estimate)
	variances <- c(variances, var(n.sample))
}

estimates
variances

# part 2

sample.size <- 1000

exp.sample <- rexp(sample.size)
estimates <- c()
for (n in 1:sample.size) {
	n.sample <- exp.sample[1:n]
	n.estimate <- sum(n.sample^5 * cos(n.sample)) / n
	estimates <- c(estimates, n.estimate)
}

estimates

# part 3

sample.size <- 1000
test.reps <- 1

x.sample <- 4 * (runif(sample.size) - 0.5)
y.sample <- 4 * (runif(sample.size) - 0.5)
complex.sample <- complex(real = x.sample, imaginary = y.sample)
in.mandelbrot <- rep(TRUE, sample.size)

for (i in 1:sample.size) {
	orbit.point <- complex(real = 0, imaginary = 0)
	for (j in 1:test.reps) {
		orbit.point <- orbit.point ^ 2 + complex.sample[i]
		if (Mod(orbit.point) > 2) {
			in.mandelbrot[i] = FALSE
			break
		}
	}
}

estimate <- sum(in.mandelbrot, na.rm = TRUE) / sample.size
estimate
