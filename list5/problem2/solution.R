# part b)

x <- 1

f <- function(x) {
	return(x * abs(log(x)) / (1 + x) * exp(-sqrt(x) - x))
}

gen.next <- function(x) {
	y <- rgamma(1, shape = 2, rate = x)
	acc.prob <- min(1, f(y) * dgamma(x, shape = 2, rate = y) / f(x) / dgamma(y, shape = 2, rate = x))
	if (is.nan(acc.prob) || runif(1) < acc.prob) {
		return(y)
	} else {
		return(x)
	}
}

generate <- function(x, n) {
	samples <- c()
	burn.in <- 10000
	between.time <- 1000
	for (i in 1:burn.in) {
		x <- gen.next(x)
	}
	samples <- c(samples, x)
	for (i in 1:(n - 1)) {
		for (j in 1:between.time) {
			x <- gen.next(x)
		}
		samples <- c(samples, x)
	}
	return(samples)
}

samples <- generate(x, 1000)
samples
numerator <- ifelse(samples >= 1, 1, -1)
denominator <- exp(-samples) / unlist(lapply(samples, f))
sum(numerator)/sum(denominator)

# part c)

x <- 1

f <- function(x) {
	return((cos(x) + x)^(-3))
}

q <- function() {
	return(sqrt(1 / (1 - runif(1))) - 1)
}

dq <- function(y) {
	return((1 + y)^(-3))
}

gen.next <- function(x) {
	y <- q()
	acc.prob <- min(1, f(y) * dq(x) / f(x) / dq(y))
	if (is.nan(acc.prob) || runif(1) < acc.prob) {
		return(y)
	} else {
		return(x)
	}
}

generate <- function(x, n) {
	samples <- c()
	burn.in <- 10000
	between.time <- 1000
	for (i in 1:burn.in) {
		x <- gen.next(x)
	}
	samples <- c(samples, x)
	for (i in 1:(n - 1)) {
		for (j in 1:between.time) {
			x <- gen.next(x)
		}
		samples <- c(samples, x)
	}
	return(samples)
}

samples <- generate(x, 1000)
samples

# part d)

rho <- 0.5

gen.next <- function(x, i) {
	x[i] <- rnorm(1, mean = rho * x[3 - i], sd = sqrt(1 - rho^2))
	return(x)
}

generate <- function(x, n) {
	samples <- list()
	burn.in <- 10000
	between.time <- 2
	for (i in 1:burn.in) {
		x <- gen.next(x, 1 + i %% 2)
	}
	samples[[length(samples) + 1]] <- x
	for (i in 1:(n - 1)) {
		for (j in 1:between.time) {
			x <- gen.next(x, 1 + j %% 2)
		}
		samples[[length(samples) + 1]] <- x
	}
	return(samples)
}

samples <- generate(c(0, 0), 1000)
samples
x <- sapply(samples, function(a) a[1])
y <- sapply(samples, function(a) a[2])
cov(x, y)
png("normal.png")
plot(x, y)
dev.off()
