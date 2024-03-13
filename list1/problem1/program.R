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

for (no_repeats in c(50, 500, 1000)) {
	m <- 37
	a <- 19
	no_values1 <- no_repeats
	random_values1 <- generate(a, m, no_values1)
	print(random_values1) # period of the first generated is equal to 36

	# second generator

	m <- 2^31 - 1
	a <- 39373
	no_values2 <- no_repeats
	random_values2 <- generate(a, m, no_values2)
	print(random_values2)

	# plotting the numbers on the number line and drawing the frequency histogram

	png(paste("generator1line_", no_repeats, ".png", sep = ""))
	plot(random_values1, rep(0, no_values1), main = paste("Rozmieszczenie punktów z pierwszego generatora na odcinku\n", no_repeats, " pierwszych liczb", sep = ""), xlab = "wartość z odcinka", ylab = "")
	abline(h = 0)
	dev.off()

	png(paste("generator1hist_", no_repeats, ".png", sep = ""))
	hist(random_values1, breaks = 10, probability = TRUE, main = paste("Histogram pierwszego generatora\n", no_repeats, " pierwszych liczb", sep = ""), xlab = "zakresy", ylab = "prawdopodobieństwo")
	abline(h = 1, col = "green", lwd = 3)
	dev.off()

	png(paste("generator2line_", no_repeats, ".png", sep = ""))
	plot(random_values2, rep(0, no_values2), main = paste("Rozmieszczenie punktów z drugiego generatora na odcinku\n", no_repeats, " pierwszych liczb", sep = ""), xlab = "wartość z odcinka", ylab = "")
	abline(h = 0)
	dev.off()

	png(paste("generator2hist_", no_repeats, ".png", sep = ""))
	hist(random_values2, breaks = 10, probability = TRUE, main = paste("Histogram drugiego generatora\n", no_repeats, " pierwszych liczb", sep = ""), xlab = "zakresy", ylab = "prawdopodobieństwo")
	abline(h = 1, col = "green", lwd = 3)
	dev.off()

	# plotting the number pairs from one generator on the square to visualise dependency

	png(paste("dependent1_", no_repeats, ".png", sep = ""))
	plot(random_values1[2 * 1:(no_values1 / 2) - 1], random_values1[2 * 1:(no_values1 / 2)], main = paste("Rozmieszczenie punktów z pierwszego generatora na kwadracie\n", no_repeats, " pierwszych liczb", sep = ""), xlab = "pierwsza współrzędna", ylab = "druga współrzędna")
	dev.off()

	png(paste("dependent2_", no_repeats, ".png", sep = ""))
	plot(random_values2[2 * 1:(no_values2 / 2) - 1], random_values2[2 * 1:(no_values2 / 2)], main = paste("Rozmieszczenie punktów z drugiego generatora na kwadracie\n", no_repeats, " pierwszych liczb", sep = ""), xlab = "pierwsza współrzędna", ylab = "druga współrzędna")
	dev.off()

	# plotting the number pairs from two used generators

	#png(paste("independent_", no_repeats, ".png", sep = ""))
	#plot(random_values1, random_values2)
	#dev.off()
}
