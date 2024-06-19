# part b)

sizes <- c(50, 100, 200)
alpha <- 0.05
no.reps <- 10000

for (n in sizes) {
	d.values <- c()
	for (i in 1:no.reps) {
		samples <- runif(n)
		ks.result <- ks.test(samples, punif)
		d.values <- c(d.values, ks.result$statistic)
	}
	sorted <- sort(d.values, decreasing = TRUE)
	print(n)
	print(sorted[alpha * no.reps])
}

# part c)

no.rejected <- 0

for (i in 1:10000) {
	samples <- rbeta(50, shape1 = 2, shape2 = 0.5)
	ks.result <- ks.test(samples, punif)
	if (ks.result$p.value < alpha) {
		no.rejected <- no.rejected + 1
	}
}

print("beta(2, 0.5)")
print(no.rejected)

no.rejected <- 0

for (i in 1:10000) {
	samples <- rbeta(50, shape1 = 1.5, shape2 = 2)
	ks.result <- ks.test(samples, punif)
	if (ks.result$p.value < alpha) {
		no.rejected <- no.rejected + 1
	}
}

print("beta(1.5, 2)")
print(no.rejected)
