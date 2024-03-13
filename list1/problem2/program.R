for (no.repeats in c(50, 500, 1000)) {

	# set the randomisation seed

	set.seed(1)

	# generate the random pairs

	random.values = runif(2 * no.repeats)
	x.values = random.values[2 * 1:no.repeats - 1]
	y.values = random.values[2 * 1:no.repeats]

	# plot the pairs on a square

	png(paste("square_", no.repeats, ".png", sep = ""))
	plot(x.values, y.values, xlim = c(0, 1), ylim = c(0, 1), main = "rozmieszczenie par punktów\nz generatora wbudowanego na kwadracie", xlab = "pierwsza współrzędna", ylab = "druga współrzędna")
	dev.off()

	# plot the density

	grid.size = 3
	occurences = matrix(0, grid.size, grid.size)
	for (i in 1:no.repeats) {
		occurences[1 + floor(grid.size * x.values[i]), 1 + floor(grid.size * y.values[i])] = occurences[1 + floor(grid.size * x.values[i]), 1 + floor(grid.size * y.values[i])] + 1
	}
	occurences <- c(occurences)

	png(paste("frequency_", no.repeats, ".png", sep = ""))
	plot(1:grid.size^2, occurences / no.repeats * grid.size^2, type = "l", ylim = c(0, 2), main = "Porównanie rozkładu łącznego z gęstością\nrozkładu jednostajnego", xlab = "numer kwadratu", ylab = "iloraz liczby przynależących punktów i spodziewanej wartości teoretycznej")
	abline(h = 1, col = "green", lwd = 3)
	dev.off()
}
