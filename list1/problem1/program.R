# generator function

generate <- function(a, m, no_values) {
	values = c(1)
	next_value = a %% m
	for (i in 2:no_values) {
		values <- c(values, next_value)
		next_value = (a * next_value) %% m
	}
	return(values / m)
}

# first generator

m <- 37
a <- 19
no_values1 <- 50
random_values1 <- generate(a, m, no_values1)
print(random_values1) # period of the first generated is equal to 36

# second generator

m <- 2^31 - 1
a <- 39373
no_values2 <- 50
random_values2 <- generate(a, m, no_values2)
print(random_values2)

# plotting the numbers on the number line

plot(random_values1, rep(0, no_values1))
abline(h = 0)
plot(random_values2, rep(0, no_values2))
abline(h = 0)

# plotting the number pairs from one generator on the square to visualise dependency

plot(random_values1[2 * 1:(no_values1 / 2) - 1], random_values1[2 * 1:(no_values1 / 2)])
plot(random_values2[2 * 1:(no_values2 / 2) - 1], random_values2[2 * 1:(no_values2 / 2)])

# plotting the number pairs from two used generators

plot(random_values1, random_values2;
