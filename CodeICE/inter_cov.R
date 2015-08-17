rm(list=ls())
require(inline)
require(rbenchmark)

include <- c("#include </home/josue/ICE/CodeICE/file_cov.hpp>")

codeCov <- '        
        Cov cov(as<mat>(A));
        return wrap(cov.CovarianceOfMatrix());
        '
CovarianceOfMatrix <- cxxfunction(signature(A="numeric"),
                                body=codeCov,
                                includes = include,
                                plugin="RcppArmadillo")

#TestMatrix <- matrix(runif(36), ncol=6)

M = matrix(c(1,0,0.1,0,
             0,1,1.1,0.6,
             0.1,0.8,1,0.4,
             0.1,0.8,0.8,1), 
           nrow = 4,
           byrow = T)
CovarianceOfMatrix(M)
