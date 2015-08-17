SignStable <- function(M){
        
}

SignSemistable <- function(M){
    if (!AlfaCondition(M)){
            #print("<Fail alfa condition!>")
            return(FALSE)
    }
    if (!BetaCondition(M)){
            #print("<Fail beta condition!>")
            return(FALSE)
    }
    if (!GammaCondition(M)){
            #print("<Fail gamma condition!>")
            return(FALSE)
    }
    return(TRUE)
}

SignQuasistable <- function(M){
        
}

SD <- function(M){        
        #signed_digraph
        M[ which(M<0) ] <- -1
        M[ which(M>0) ] <- +1
        return(M)
}

G <- function(M){
        #undirected graph
        #simetric
        M[which(M!=0)] <- 1
        n <- nrow(M)
        for (i in 1:(n-1)){
                for (j in (i+1):n){
                        if ( M[i,j] != M[j,i] ){
                                M[i,j] <- 0
                                M[j,i] <- 0
                        }
                }
        }
        return(M)
}

AlfaCondition <- function(M){
        n <- nrow(M)
        for(i in 1:n){
                if( M[i,i] > 0){
                        return(FALSE)
                }
        }
        return(TRUE)
}

BetaCondition <- function(M){
        n <- nrow(M)
        for (i in 1:(n-1)){
                for (j in (i+1):n){
                        if ( M[i,j]*M[j,i] > 0 ){
                                return(FALSE)
                        }
                }
        }
        return(TRUE)
}

PotencialCycle <- function(M,i){
        n <- nrow(M)
        to <- NULL
        from <- NULL
        for (j in 1:n){
                if(i!=j){
                        if(M[j,i] != 0){
                                to <- c(to, j)
                        }
                        if(M[i,j] != 0){
                                from <- c(from, j)
                        }       
                }                
        }
        for (to_ in to){
                for (from_ in from){
                        if(to_ != from_){
                                return(TRUE)
                        }
                }
        }
        return(FALSE)
}

GammaCondition <- function(M){
        DFS_Visitar <- function(l,i){                
                l$state[i] <- TRUE                
                for (j in 1:l$n){
                        if (l$M[j,i] != 0 &  j != l$father[i] & i!=j ){
                                if (l$state[j] == FALSE){
                                        l$father[j] <- i
                                        l <- DFS_Visitar(l,j)
                                        if(l$result == TRUE){
                                                return(l)
                                        } 
                                }else{                                      
                                        l$result <- TRUE
                                        return(l)
                                }
                        }
                }
                return(l)             
        }
        
        n <- nrow(M)
        M[which(M!=0)] <- 1
        diag(M) <- 0
        l <- list(M = M,
                  n = n,
                  father = rep(0,n),
                  state = rep(FALSE,n),                  
                  result = FALSE)
        for (i in 1:n){
                if ( l$state[i] == FALSE & PotencialCycle(l$M,i) ){
                        l <- DFS_Visitar(l,i)
                        if( l$result == TRUE ){
                                return(FALSE)
                        } 
                }
        }
        return(TRUE)
}

TestSemiStable1 <- function(){
        require(gtools)
        p <- permutations(n = 3,r = 4,v = c(-1,0,1),repeats.allowed = T)
        n <- nrow(p)
        eigenvalues <- NULL
        teo <- NULL
        for (i in 1:n){
                M <- matrix(p[i,], nrow = 2)                
                if(Re(eigen(M)$values[1]) <= 0 & Re(eigen(M)$values[2]) <= 0){
                        eigenvalues <- TRUE
                }
                else{
                        eigenvalues <- FALSE
                }
                teo <- SignSemistable(M)
                if (!teo){
                        print(eigenvalues)
                        print(M)       
                }                
        }
}

TestSemiStable2 <- function(){
        require(gtools)
        p <- permutations(n = 3,r = 9,v = c(-1,0,1),repeats.allowed = T)
        n <- nrow(p)
        eigenvalues <- NULL
        teo <- NULL
        for (i in 1:n){
                M <- matrix(p[i,], nrow = 3)                
                if(Re(eigen(M)$values[1]) <= 0 & Re(eigen(M)$values[2]) <= 0){
                        eigenvalues <- TRUE
                }
                else{
                        eigenvalues <- FALSE
                }
                teo <- SignSemistable(M)
                if (teo & !eigenvalues){                        
                        print(M)       
                }               
        }
}