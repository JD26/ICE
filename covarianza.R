
A <- matrix (c(1,0,.1,0,
	0,1,1.1,0.6,
	0.1,0.8,1,0.4,
	0.1,0.8,0.8,1), nrow=4, byrow=T)

rowmeans <- function(A) {
	#a(.,i)
	n <- nrow(A)
	c = NULL
	for (i in 1:n) {
		s <- 0
		for ( j in 1:n) {
			if (i != j) {
				s <- s + A[i,j]
			}
		}		
		c <- c(c, s/(n-1))
	}
	return(c)
}

colmeans <- function(A) {
	#a(i,.)
	n <- nrow(A)
	c = NULL
	for (i in 1:n) {
		s <- 0
		for ( j in 1:n) {
			if (i != j) {
				s <- s + A[j,i]
			}
		}		
		c <- c(c, s/(n-1))
	}
	return(c)
}


productcross <- function(A) {
	v <- rowmeans(A)
	w <- colmeans(A)
	n <- length(v)
	s <- 0
	for (i in 1:n) {
		s <- s + v[i]*w[i]
	}
	return(s)
}