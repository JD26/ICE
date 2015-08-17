rm(list = ls())

M <- matrix(c(2,3,20000,5), nrow=2)

Inverse <- function(A) {
	n <- nrow(A)
	Bk_1 <- diag(n)
	for ( i in 1:(n-1) ) {
		ak <- sum(diag(A%*%Bk_1))/i
		Bk <- -A%*%Bk_1 + ak*diag(n)
		Bk_1 <- Bk
	}
	an <- sum(diag(A%*%Bk_1))/n
	return(Bk_1/an)
}