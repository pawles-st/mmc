# part 2

sample.size <- 1000

gen.bernoulli <- function(p) {
	if (runif(1) < p) {
		return(1)
	} else {
		return(0)
	}
}

p = 0.5
bernoulli.sample <- unlist(lapply(1:sample.size, function(x) gen.bernoulli(p)))
estimates <- c()

for (n in 1:sample.size) {
	n.sample <- bernoulli.sample[1:n]
	estimate <- sum(n.sample) / n
	estimates <- c(estimates, estimate)
}
estimates[c(rep(FALSE, 99), TRUE)]

# part 3

sample.size <- 1000
n <- 1000
	
estimates <- c()

for (i in 1:sample.size) {
	unif.sample <- runif(n)
	estimate <- sum(unif.sample) > 0.5 * n + 0.1 * sqrt(n)
	estimates <- c(estimates, estimate)
}

pbb <- sum(estimates, na.rm = TRUE) / sample.size
