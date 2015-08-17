rm(list = ls())
A <- matrix(c(-1,1,1,
                  -1,0,0,
                  0,-1,0),nrow=3, byrow =T)

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
for (j in 1:n) {
        count <- 0
        L <- NULL
        for (i in 1:n) {
                if (i != j & A[i, j] != 0) {
                        L <- c(L, i)
                        count <- count + 1
                }
        }
        if (count > 0) {
                FIRST[j+1] <- FIRST[j] + count
                ADJ[FIRST[j]:(FIRST[j+1]-1)] <- L
        }else {
                FIRST[j+1] <- FIRST[j]
        }
}

for (j in 1:n) {
        I <- NULL
        if (FIRST[j] <  FIRST[j+1]) {
            I <- ADJ[FIRST[j]:(FIRST[j+1]-1)]    
        }
        l <- FIRST[j] - 1
        for (i in I) {
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
        #print("Falló el test COUNT1!")
}

l <- NULL
for (i in 1:n) {
        l <- c(l, abs(SELF[i]) + FIRST[i + 1] - FIRST[i])
}
if ( length(which(l==2)) <2  & length(which(l==1)) <1 ) {
        #Condiciones necesarias para sign stabilidad
        #print("Falló el test COUNT2!")
}

TAG <- rep(NULL, e)
for (j in 1:n) {
        I <- NULL
        if (FIRST[j] <  FIRST[j+1]) {
            I <- ADJ[FIRST[j]:(FIRST[j+1]-1)]    
        }
        l <- FIRST[j] - 1
        for (i in I) {
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


Visited <- rep(FALSE, n)
Father <- rep(0,n)

IsAffected <- function(x) {
    for (i in 1:e) {
        if (x == ADJ[i]) {
            return(TRUE)
        }
    }
    return(FALSE)
}

Affect <- function(x) {
    if ( FIRST[x] < FIRST[x+1] ) {
        return(TRUE)
    }
    return(FALSE)
}


SearchLoop <- function(Sp) {
    Visited[Sp] <<- TRUE
    AffectedBySp <<- ADJ[FIRST[Sp]:(FIRST[Sp+1]-1)]
    for (SpAffected in AffectedBySp) {
        if ( SpAffected != Father[Sp] ) {
            if ( !Visited[SpAffected] ) {
                Father[SpAffected] <<- Sp
                return(SearchLoop(SpAffected))
            } else {
                return(TRUE)
            }
        }
    }
    return(FALSE)
}

for (i in 1:n) {
            if (!Visited[i] & IsAffected(i) & Affect(i) ) {                    
                       if ( SearchLoop(i) ) {
                            return(FALSE)
                       }                   
            }
}

return(TRUE)