sign <- function(p) {
	n <- length(p)
	visited <- rep(FALSE, n)
	sgn <- 1
	for (k in 1:n) {
		if (!visited[k]) {
			ext <- k
			L <- 0
			while (!visited[ext]) {
				L <- L+1
				visited[ext] <- TRUE
				ext <- p[ext]
			}
			if ((L%%2) == 0 ) {
				sgn <- -sgn
			}
		}
	}
	return(sgn)
}

p = c(1,2,3,4,5)
print(sign(p))
p = c(1,2,3,5,4)
print(sign(p))
p = c(1)
print(sign(p))

