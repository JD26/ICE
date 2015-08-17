# Functions for Feedbacks of a matrix
# and coeficients of characteristic
# polinomial
# References:
# http://math.stackexchange.com/questions/65923/how-does-one-compute-the-sign-of-a-permutation/72221#72221
# Matrix Theory Zhan, 2003

rm(list = ls())
library(gtools)
library(pracma)


f <- function(A, B) {
	n <- length(A)
	resp <- ""
	for (i in 1:n){
		resp <- paste0(resp,"a(",A[i],",",B[i],")")
	}
	return(resp)
}

ChangeIndex <- function(original, permutation) {
	n <- length(permutation)
	newp <- NULL
	for (p in permutation) {
		for (j in 1:n) {
			if (p == original[j]) {
				newp <- c(newp, j)
				break
			}
		}
	}
	return(newp)
}

Sign <- function(original, permutation) {
	p <- ChangeIndex(original, permutation)
	if (Sign0(p) == -1) {
		return("-")
	}
	if (Sign0(p) == 1) {
		return("+")
	}
}

Sign0 <- function(p) {
	# p is a permutation de {1,2,...,n}
	# sign(p) = (-1)^(No. of even-length cycles)
	# Complexity : O(n + ncycles) ~ O(n + Hn) ~~ O(n+log(n)) steps
	# Derek O'Connor 20 March 2011
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

Feedback <- function(k = 1,n = 3) {
	# Return of feedbacks of a matrix
	# of size nxn.
	# Is simbolic.
	Cs <- combinations(n, k)
	nc <- nrow(Cs)
	Fk <- ""
	for (i in 1:nc) {
		Ps <- permutations(k, k, Cs[i,])
		np <- nrow(Ps)
		for (j in 1:np) {
			a <- f(Cs[i,],Ps[j,])				
			Fk <- paste(Fk, a, sep= Sign(Cs[i,],Ps[j,]))
		}
	}
	print(Fk)
}

MessureFeedbacks <- function(A) {
	F <- FeedbacksOfMatrix(A)
	if (length(F)>3){
		for (i in 3:(length(F)-1) ) { 
			resp = F[i-1]*F[i] - F[i-2]*F[i+1]
			print(resp)
		}
	}
}

Matrix_Fi <-function(f,m){
  # m debe ser mayor o igual que 1
  Mf = matrix( rep(0,m*m), nrow=m)  
  p = length(f)
  for ( i in 1:m ){
    k = 1
    j = 2*i
    while (j>=1 & k<=m){
      if (j <= p){
        Mf[k,i]=f[j]
      }        
      k = k+1  
      j = j-1  
    }
  }
  return(Mf)
}

FeedbacksOfMatrix <- function(A) {
	# Calculated of Feedbacks de A
	# from definition in terms of
	# combinations and permutations
	n <- nrow(A)
	Fbs <- c(-1)
	for (k in 1:n) {
		Fbs <- c(Fbs, FeedbackLevelK(A, n, k))
	}
	return(Fbs)
}

FeedbackLevelK <- function(A, n, k) {
	Cs <- combinations(n, k)
	nc <- nrow(Cs)
	Fb <- 0
	for (i in 1:nc) {
		Fb <- Fb + SumOfProducts(A,k,Cs[i,])
	}
	return((-1)^(k+1)*Fb)
}

SumOfProducts <- function(A,k,Cs) {
	Ps <- permutations(k, k, Cs)
	np <- nrow(Ps)
	SP <- 0
	for (i in 1:np) {
		SP <- SP + SignOfPermutation(Cs,Ps[i,])*ProductOfElements(A,k,Cs,Ps[i,])
	}
	return(SP)
}

ProductOfElements <- function(A,k,Cs,Ps) {
	P <- 1
	for (i in 1:k) {
		P <- P*A[Cs[i],Ps[i]]
	}
	return(P)
}

SignOfPermutation <- function(original, permutation) {
	p <- ChangeIndex(original, permutation)
	return(Sign0(p))
}

Fi_poly <- function(Mx){ 
  # Calculate the coeficients of
  # characteristic poly of Mx
  # Algorithm :
  # Faddeev-Leverrier
  n = dim(Mx)[1]
  A = Mx
  c = rep(0,n)
  for ( i in 1:(n-1) ){
    c[i]= -sum(diag(A))/i
    A = Mx%*%( A + diag( rep(c[i],n) ) )    
  }
  c[n]= -sum(diag(A))/n  
  c = c(1,c)
  return(c)
}

BestMatrix <- function(n) {
	E <- matrix(rep(0,n*n), ncol = n)
	for (i in 2:n) {
		E[i,i-1] = 1
	}
	return(E)
}

CHACMAlgorithm <- function(A) {
	n <- nrow(A)
	E <- BestMatrix(n)
	A <- A - E
	# Hay que trabajar...
}



Test <- function() {
	A <- matrix(c(1,2,4,22,1,2,5,2,1), nrow=3)
	A <- matrix(runif(64), ncol=8)		

	print("#############################################")
	print("Using 'FeedbacksOfMatrix' function:")	
	print(system.time(Fs <- FeedbacksOfMatrix(A)))
	#print(-1*Fs)

	print("#############################################")
	print("Using 'charpoly' function (pracma):")	
	print(system.time(Cp <- charpoly(A)))		
	#print(Cp)

	print("#############################################")
	print("Using 'Fi_poly' function (Faddeev-Leverrier):")	
	print(system.time(Fp <- Fi_poly(A)))	
	#print(Fp)
}

#Test()