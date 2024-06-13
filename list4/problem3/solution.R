# part 1

reps <- 1:200
estimates <- c()

sample.size <- 100

for (i in reps) {
	estimate <- sum(rexp(sample.size, rate = 2)) / sample.size
	estimates <- c(estimates, estimate)
}

estimates
sd(estimates)

# part 2

reps <- 1:200
estimates <- c()

sample.size <- 100

for (i in reps) {
	estimate <- sample.size / sum(rexp(sample.size, rate = 2))
	estimates <- c(estimates, estimate)
}

estimates
sd(estimates)
