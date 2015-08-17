rm(list=ls())
require(inline)
require(rbenchmark)

include <- c("#include </home/josue/ICE/CodeICE/file_poly.hpp>")

codeFL <- '        
        Poly poly(as<mat>(A));
        return wrap(poly.FaddeevLeverrier());
        '
FaddeevLeverrier <- cxxfunction(signature(A="numeric"),
                                body=codeFL,
                                includes = include,
                                plugin="RcppArmadillo")

TestMatrix <- matrix(runif(36), ncol=6)
FaddeevLeverrier(TestMatrix)
