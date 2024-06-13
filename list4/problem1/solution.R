reps <- 1:1000

first.estimates <- c()
second.estimates <- c()

samples <- c()
for (n in reps) {
	samples <- c(samples, rcauchy(1))
	first.estimates <- c(first.estimates, length(samples[samples > 3]) / n)
}

first.estimates
var(first.estimates)

samples <- c()
for (n in reps) {
	samples <- c(samples, -3 / runif(1))
	second.estimates <- c(second.estimates, sum(dcauchy(samples) * samples^2 / 3) / n)
}

second.estimates

png("cauchy.png")
plot(reps, first.estimates, type = "l", main = "porównanie estymatorów Monte Carlo dla P(X > 3)", ylim = c(0, 0.2), col = "blue", xlab = "repetitions", ylab = "probability estimate")
lines(reps, second.estimates, type = "l", col = "red")
legend(x = "topright", legend = c("Estymator standardowy", "IS1"), lty = c(1, 1), col = c("blue", "red"))
dev.off()
