# Bernoulli distribution generator

generate.bernoulli <- function(p) {
	random.value <- runif(1)
	if (random.value < p) {
		return(1)
	} else {
		return(0)
	}
}

# Binomial distribution generator

#generate.binomial <- function(n, p) {
#	random.value <- runif(1)
#	k = 0
#	n.choose.k <- 1
#	cdf <- 0
#	while (cdf < random.value) {
#		cdf <- cdf + n.choose.k * p^k * (1 - p)^(n - k)
#		k <- k + 1
#		n.choose.k <- n.choose.k * (n - k + 1) / k
#	}
#	return(k)
#}

generate.binomial <- function(n, p) {
	successes <- 0
	for (i in 1:n) {
		if (generate.bernoulli(p) == 1) {
			successes <- successes + 1
		}
	}
	return(successes)
}

# Poisson distribution generator

generate.poisson <- function(lambda) {
	random.value <- runif(1)
	k <- 0
	mass <- exp(-lambda)
	cdf <- 0
	while (cdf < random.value) {
		cdf <- cdf + mass
		k <- k + 1
		mass <- mass * lambda / k
	}
	return(k)
}

# testing

no.repeats <- 10000

bernoulli.numbers <- 0:1

for (i in 1:no.repeats) {
	bernoulli.numbers <- c(bernoulli.numbers, generate.bernoulli(0.7))
}

png("bernoulli.png")
hist(bernoulli.numbers, breaks = 2, probability = TRUE, xlim = c(0, 1), main = "Histogram generatora dla rozkładu Bernoulliego z parametrem p = 0.7", xlab = "argument", ylab = "wartość funkcji masy")
dev.off()

binomial.numbers <- 0:10

for (i in 1:no.repeats) {
	binomial.numbers <- c(binomial.numbers, generate.binomial(10, 0.7))
}

png("binomial.png")
hist(binomial.numbers, breaks = 10, probability = TRUE, xlim = c(0, 10), main = "Histogram generatora dla rozkładu dwumianowego z parametrami n = 10, p = 0.7", xlab = "argument", ylab = "wartość funkcji masy")
dev.off()

poisson.numbers <- 0:10

for (i in 1:no.repeats) {
	poisson.numbers <- c(poisson.numbers, generate.poisson(0.7))
}

png("poisson.png")
hist(poisson.numbers, breaks = 10, probability = TRUE, xlim = c(0, 10), main = "Histogram generatora dla rozkładu Poissona z parametrem lambda = 0.3", xlab = "argument", ylab = "wartość funkcji masy")
dev.off()
