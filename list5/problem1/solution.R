moves1 <- c(2, 3) # first room
moves2 <- c(1, 4) # second room
moves3 <- c(1, 4) # third room
moves4 <- c(2, 3, 7) # fourth room
moves5 <- c(6) # fifth room
moves6 <- c(5, 7) # sixth room
moves7 <- c(4, 6) # seventh room

moves <- matrix(list(moves1, moves2, moves3, moves4, moves5, moves6, moves7), nrow = 1)

move <- function(n) {
	if (length(moves[[n]]) == 1) {
		return(moves[[n]])
	} else {
		return(sample(moves[[n]], 1, replace = TRUE))
	}
}

no.moves <- 10000
current.room <- 1
rooms.visited  <- c(current.room)

for (i in 1:no.moves) {
	current.room <- move(current.room)
	rooms.visited <- c(rooms.visited, current.room)
}

# steady state: [2/14, 2/14, 2/14, 3/14, 1/14, 2/14, 2/14]

# weak ergodicity

print("room frequency")
for (i in 1:7) {
	print(length(rooms.visited[rooms.visited == i]) / no.moves)
}

# SLLN

print("room number average")
print(sum(rooms.visited) / no.moves)


