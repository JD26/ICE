rm(list = ls())
TestA <- matrix(c(-1,1,1,
                  -1,0,0,
                  0,-1,0),nrow=3, byrow =T)

TestAlfaBetaGamma <- function(A = TestA) {
        n <- nrow(A)
        SELF <- rep(0, n)
        FIRST <- rep(0, n+1)
        L <- rep(0, n)
        for (i in 1:n) {
                SELF[i] <- sign(A[i,i])
                if (SELF[i] > 0) {
                        #print("<RETROACCION POSITIVA>")		
                        return(FALSE)
                }
        }
        e <- 0
        for (i in 1:n) {
                for (j in 1:n) {
                        if (i != j & A[i,j] != 0){
                                e <- e + 1
                        }
                }
        }
        ADJ <- rep(0, e)
        SIGN <- rep(NULL, e)
        FIRST[1] <- 1
        FIRST[n + 1] <- e + 1
        for (i in 1:n) {
                count <- 0
                L <- NULL
                for (j in 1:n) {
                        if (i != j & A[i, j] != 0) {
                                L <- c(L, j)
                                count <- count + 1
                        }
                }
                if (count > 0) {
                        FIRST[i+1] <- FIRST[i] + count
                        ADJ[FIRST[i]:(FIRST[i+1]-1)] <- L
                }else {
                        FIRST[i+1] <- FIRST[i]
                }
        }
        
        for (i in 1:n) {
                J <- NULL
                if (FIRST[i] <  FIRST[i+1]) {
                    J <- ADJ[FIRST[i]:(FIRST[i+1]-1)]    
                }
                l <- FIRST[i] - 1
                for (j in J) {
                        l <- l + 1                        
                        if (A[i, j] > 0) {
                                SIGN[l] <- TRUE
                        }
                        else {
                                SIGN[l] <- FALSE
                        }
                }
        }
        
        if (e > (n*n/2) ) {
                #Condiciones necesarias para sign stabilidad
                print("Falló el test COUNT1!")
        }
        
        l <- NULL
        for (i in 1:n) {
                l <- c(l, abs(SELF[i]) + FIRST[i + 1] - FIRST[i])
        }
        if ( length(which(l==2)) <2  & length(which(l==1)) <1 ) {
                #Condiciones necesarias para sign stabilidad
                print("Falló el test COUNT2!")
        }
        
        TAG <- rep(NULL, e)
        for (i in 1:n) {
                J <- NULL
                if (FIRST[i] <  FIRST[i+1]) {
                    J <- ADJ[FIRST[i]:(FIRST[i+1]-1)]    
                }
                l <- FIRST[i] - 1
                for (j in J) {
                        l <- l + 1
                        if (A[i,j] != 0) {
                                if ( A[j,i] == 0) {
                                        TAG[l] = FALSE
                                } else {
                                        TAG[l] = TRUE
                                }
                        }
                }
        }
        
        NEW <- rep(0, n)
        FATHER <- rep(0, n)
        LOC <- rep(0, n)
        
        SEARCH <- function(v) { 
                print(paste("entra",v))  
                                                
                while( NEW[v] < (FIRST[v+1] - 1) ) {
                        NEW[v] <<- NEW[v] + 1		
                        l <<- NEW[v]
                        TAG[l] <<- FALSE
                        j <<- ADJ[l]
                        print(v)
                                print(FATHER)
                                print(NEW)
                                print(FIRST)
                                print(j)
                                print(ADJ)
                        if ( NEW[j] < FIRST[j] ) {                            
                                LOC[j] <<- l
                                FATHER[j] <<- v
                                SEARCH(j)
                                #if( !SEARCH(j) ){
                                #    return(FALSE)
                                #}
                        } else if ( NEW[j] != FIRST[j+1] ) {
                                
                                if ( (j != FATHER[v]) | (SIGN[LOC[v]]==SIGN[l]) ) { 
                                        print("hooola_____________")
                                        #return(FALSE)
                                } else {
                                        TAG[l]  <<- TRUE		
                                        TAG[LOC[v]] <<- TAG[l]			
                                }
                        }		
                }
                NEW[v] <<- FIRST[v+1]
                #return(TRUE)
        }
        
        for (i in 1:n) {
                NEW[i] <- FIRST[i] - 1
        }

        for (i in 1:n) {
            
                        
                if (NEW[i] < FIRST[i]) {
                        SEARCH(i)
                        #if( !SEARCH(i) ){                            
                        #    return(FALSE)
                        #}
                }
        }	
        #return(TRUE)
}



