rm(list=ls())
require(inline)
require(rbenchmark)

include <- c("#include </home/josue/ICE/CodeICE/file_varauto.hpp>")

codeVarAuto <- '        
        VarAuto varauto(as<mat>(A));
        return wrap(varauto.VarOfDiag());
        '
VarianceOfAuto <- cxxfunction(signature(A="numeric"),
                                  body=codeVarAuto,
                                  includes = include,
                                  plugin="RcppArmadillo")

M = matrix(c(2,-2,sqrt(6),
             -2,5,3,
             sqrt(6),-1,1), 
           nrow = 3,
           byrow = T)

M = matrix(c(2,sqrt(6),
             sqrt(6),1), 
           nrow = 2,
           byrow = T)

VarianceOfAuto(M)
