#DESCRIPCION
#CODIGO QUE IMPLEMENTA EL ALGORITMO PARA MATRICES SIGN
#ESTABLES. EL ALGORITMO ES DEL LIBRO ... 

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
filter2 <- function(A){
        
        DigA <- function(A){
                diag(A) = 0
                A[which(A!=0)]=1
                n <- nrow(A)
                for (i in 1:(n-1)){
                        for (j in (i+1):n){
                                value <- max(A[i,j],A[j,i])
                                A[i,j] <- value
                                A[j,i] <- value
                        }
                }
                return(A)
        }
        DFS <- function(A){
                DFS_Visitar <- function(l,j){                        
                        l$estado[j] <- "VISITADO"
                        l$tiempo <- l$tiempo+ 1
                        l$d[j] <- l$tiempo
                        for (i in 1:l$n){
                                if (l$M[j,i] != 0 & i != l$padre[j]){
                                        if (l$estado[i] == "NO_VISITADO"){
                                                l$padre[i] <- j                                        
                                                l = DFS_Visitar(l,i)
                                                if(l$result == TRUE){return(l)} 
                                        }else{                                      
                                                l$result <- TRUE
                                                return(l)
                                        }
                                }
                        }
                        l$estado[j] <- "TERMINADO"
                        l$tiempo <- l$tiempo + 1
                        l$f[j] <- l$tiempo
                        l
                }
                n <- nrow(A)
                l <- list(M = A,
                          n = n,
                          d = rep(0,n),
                          f = rep(0,n),
                          estado = rep("NO_VISITADO",n),
                          padre = rep(0,n),
                          tiempo = 0, 
                          result = FALSE)
                for (i in 1:n){
                        if (l$estado[i] == "NO_VISITADO"){
                                l <- DFS_Visitar(l,i)
                                if(l$result == TRUE){return(l)} 
                        }
                }
                return(l)
        }
        DFS_result <- function(A){                
                l <- DFS(A)
                return(l$result)
        }
        
        if (DFS_result(DigA(A)) ){
                #print("[No pasa el filtro 2]")
                return(FALSE)
        }
        #print("[Paso el filtro 2]")
        return(TRUE)
}

test_sign_semi_stable <- function(A){
        if (!filter01(A)){
                return(FALSE)
        }else if (!filter12(A)){
                return(FALSE)
        }else{
                return(TRUE)      
        } 
}


