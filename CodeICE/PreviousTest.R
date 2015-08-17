require ( rbenchmark )
require ( compiler )

library(microbenchmark)
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
        #c = c(1,c)
        return(c)
}

code <- '        
        Poly poly(as<mat>(A));
        return wrap(poly.FaddeevLeverrier());
        '
FLCpp <- cxxfunction(signature(A="numeric"),
                   body=code, 
                   includes = c("#include </home/josue/ICE/CodeICE/poly.hpp>"),
                   plugin="RcppArmadillo")
#FLCpp( matrix(c(3, 6, 2, 2, 5, 2, 1, 2, 3), 3,3) )
pos <- matrix(runif(64), ncol=8) 
microbenchmark(Cpp = FLCpp(pos),
               R = Fi_poly ( pos ),
               times =12)
res <- benchmark ( FLCpp ( pos ), 
                   Fi_poly ( pos ),
                   columns=c('test', 
                             'elapsed', 
                             'replications',
                             'relative'),
                   order = "relative",
                   replications =100)
res

