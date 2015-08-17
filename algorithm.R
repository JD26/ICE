rm(list = ls())
source("busqueda.R")
set.seed(123)
A <- matrix(c(-1,0,-1,
			  0,0,-1,
			  1,11,-1),nrow=3, byrow =T)

SampleMatrix <- function(n){
	M <- sample(c(-1, 0, 1), n^2, replace = TRUE)
	return(matrix(M, nrow = n))
}

TestFunction <- function(n, N, FUN){
		for (i in 1:N) {
			M <- SampleMatrix(n)
			#print(M)
			if(FUN(M)){				
				values <- sign(Re(eigen(M)$values))
				if ( length(which(values > 0)) > 0 ) {
					print(M)
					print(Re(eigen(M)$values))
				}
			}
			
			#print(M)
		}
}

TestFunction(500, 1, f)