library(gtools)
source('libraryICE.R')

#DESCRIPCION
#GENERA LAS MATRICES DE ADJACENCIA DE LOS ARBOLES DE N VER-
#TICES QUE SON CONECTADOS Y SIN CICLOS. LAS MATRICES RESUL-
#TANTES SON SIMETRICAS. UNA ARISTA ESTA REPRESANTADA POR 1.
#ES UN GRAFO NO DIRECCIONADO.
generator_tree <- function(n = 3){
        l <- list()
        for ( i in 1:(n-2) ){
                A <- matrix(rep(0, n*n), nrow = n)
                A[2:(n+1-i), 1] = 1
                A[1, 2:(n+1-i)] = 1
                if( i > 1 ){
                        for ( j in 2:i ){
                                A[n-i+j, j] = 1
                                A[j, n-i+j] = 1
                        }
                }
                l <- c(l, list(A))
        }
        l
}
#FILTROS DEL ALGORITMO DE MATRICES SIGN-STABLES
filter0 <- function(A){
        if(det(A) != 0){
                return(TRUE)
        }
        return(FALSE)
}
filter1 <- function(A){
        kk <- function(A){
                #Algún elemento de la diagonal es positivo?
                TRUE %in% c(diag(A)>0)
        }
        
        kj <- function(A){                
                n <- nrow(A)
                for (i in 1:(n-1)){
                        for (j in (i+1):n){
                                if (A[i,j] == A[j,i] & A[j,i] != 0){
                                        return(TRUE)
                                }
                        }
                }
                return(FALSE)
        }
        if ( kk(A) ){
                #print("[No pasa el filtro 1]")
                return(FALSE)
        }
        if ( kj(A) ){
                #print("[No pasa el filtro 1]")
                return(FALSE)
        }
        #print("[Paso el filtro 1]")
        return(TRUE)
}
test_sign_stable <- function(A){
        if (!filter0(A)){
                return(FALSE)
        }else if (!filter1(A)){
                return(FALSE)
        }
        return(TRUE)
}

#DESCRIPCION
#GENERA TODOS LOS CASOS DE GRAFOS (INCLUYENDO RETROACCIONES)
#PRUEBA LOS FILTROS DE LAS MATRICES SIGN STABLES
all_cases <- function(n = 4){
        tress <- generator_tree(n)
        nmatrix <- ifelse(n>2,n-2,1)
        for (i in 1:length(tress)) diag(tress[[i]]) = 1
        for (i in 1:length(tress)){
                ind <- which(tress[[i]]!=0)
                r <- length(ind)
                per <- permutations(n = 3, r, v = c(-1,0,1), repeats.allowed = T)
                print(nrow(per))
                dets <- c()
                for (j in 1:nrow(per)){                        
                        tress[[i]][ind] <- per[j,]
                        dets <- c( dets, test_sign_stable(tress[[i]]) )
                }
                print( length(which(dets!=FALSE)) )
        }
}