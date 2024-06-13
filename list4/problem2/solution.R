# part 1

reps <- 1:1000
estimates <- c()

sample.size <- 100

for (i in reps) {
	samples <- runif(sample.size, min = 0, max = pi)
	estimate <- sum(pi * samples * sqrt(sin(samples)) * exp(-1/2 * samples^2)) / sample.size
	estimates <- c(estimates, estimate)
}

estimates
sd(estimates)

# part 2

reps <- 1:1000
estimates <- c()

sample.size <- 100

for (i in reps) {
	samples <- sqrt(-2 * log(1 - runif(sample.size) * (1 - exp(-pi^2 / 2))))
	unif.samples <- runif(sample.size)
	estimate <- length(samples[unif.samples <= sqrt(sin(samples))]) / sample.size
	estimates <- c(estimates, estimate)
}

estimates
sd(estimates)

# part 3

reps <- 1:1000
estimates <- c()

sample.size <- 100

for (i in reps) {
	samples <- sqrt(-2 * log(1 - runif(sample.size) * (1 - exp(-pi^2 / 2))))
	estimate <- sum((1 - exp(-pi^2 / 2)) * sqrt(sin(samples))) / sample.size
	estimates <- c(estimates, estimate)
}

estimates
sd(estimates)
