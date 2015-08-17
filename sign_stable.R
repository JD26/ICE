#DESCRIPCION
#CODIGO QUE IMPLEMENTA EL ALGORITMO PARA MATRICES SIGN
#ESTABLES. EL ALGORITMO ES DEL LIBRO ... 
filter0 <- function(A){
        if(det(A) != 0){
                #print("[Paso el filtro 0]")
                return(TRUE)
        }
        #print("[No pasa el filtro 0]")
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
filter3 <- function(A){
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
        l <- list(DA = DigA(A), alfa = which(diag(A)!=0))
        #print("[Hizo el cálculo 3]")
        return(l)
}
filter4 <- function(DA, alfa){        
        adjacentes <- function(DA,v){
                return(which(DA[v,] !=0))
        }  
        test <- function(v, alfa, DA){
                for( i in v ){                        
                        if (isTRUE(adjacentes(DA,i) %in% alfa)){                                
                                return(TRUE)
                        }
                }
                return(FALSE)
        }               
        n <- nrow(DA)
        v <- c(1:n)[!(c(1:n) %in% alfa)]
        while (test(v, alfa, DA)){                
                alfa <- c(alfa,v)
                v <- c(1:n)[!(c(1:n) %in% alfa)]
        }
        #print("[Paso por el filtro 4]")
        return(alfa)        
}
filter5 <- function(DA,alfa){
        adjacentes <- function(DA,v){
                return(which(DA[v,] !=0))
        }  
        search_w <- function(v, alfa, DA){
                for (w in alfa){
                        adw <- adjacentes(DA,w)
                        if (v %in% adw){                                
                                other_adw <- (adw[!(adw %in% v)])                                
                                if (length(other_adw) == 0){
                                        return(TRUE)
                                }
                                if ( isTRUE(other_adw %in% alfa) ){                                        
                                        return(TRUE)
                                }
                        }                        
                }
                return(FALSE)
        }
        n <- nrow(DA)
        v <- c(1:n)[!(c(1:n) %in% alfa)]
        for (i in v){
                if (search_w(i, alfa, DA)){
                        #print("[Paso por el filtro 5]")
                        return(c(alfa,i))
                }
        }
        #print("[Paso por el filtro 5]")
        return(alfa)
}
filter6 <- function(alfa,n){
        if (length(alfa) == n){
                #print("[Paso el filtro 6]")
                return(TRUE)
        }
        #print("[No pasa el filtro 6]")
        return(FALSE)
}
test_sign_stable <- function(A){
        if (!filter0(A)){
                return(FALSE)
        }else if (!filter1(A)){
                return(FALSE)
        }else if (!filter2(A)){
                return(FALSE)
        }else{
                l <- filter3(A)
                while (TRUE){                        
                        l$alfa <- filter4(l$DA, l$alfa)
                        l4 <- length(l$alfa)
                        l$alfa <- filter5(l$DA, l$alfa)
                        l5 <- length(l$alfa)
                        if(l4 == l5){
                                break
                        }
                }
                return(filter6(l$alfa,nrow(A)))
        }
}

test_sign_semi_stable <- function(A){
        if (!filter1(A)){
                return(FALSE)
        }else if (!filter2(A)){
                return(FALSE)
        }else{
                return(TRUE)      
        } 
}


#M = matrix(c(0,0,-1,-1,0,0,0,-1,1,0,-1,0,1,1,0,0),nrow = 4, byrow = T)
#test_sign_stable(M)