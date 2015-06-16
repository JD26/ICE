library(gtools)
library(LoopAnalyst)
source('plot.R')
source('libraryICE.R')

names <- c("x","y","z")
central_cases <- combinations(n = 2, r = 2, v = c(1,-1), 
                             repeats.allowed =TRUE)
rows_cc <- nrow(central_cases)
lateral_cases <- permutations(n = 2, r = 2, v = c(1,-1), 
                             repeats.allowed =TRUE)
rows_lc <- nrow(lateral_cases)
aij = list()
for (i in 1:rows_cc){
        a21 <- central_cases[i,1]
        a23 <- central_cases[i,2]
        for (j in 1:rows_lc){
                a12 <- lateral_cases[j,1]
                a32 <- lateral_cases[j,2]
                mx <- c(0, a12, 0,
                       a21, 0, a23,
                       0, a32, 0)
                mx <- matrix(mx,byrow = TRUE, nrow = 3)
                #print(mx)
                aij <- c(aij, list(mx))
        }
}

rowa_aij <- length(aij)

retro_cases <- permutations(n = 3, r = 3, v = c(0,1,-1), 
                           repeats.allowed =TRUE)
rows_rc <- nrow(retro_cases)

for (i in 1:rowa_aij){
        for (j in 1:rows_rc){
                diag(aij[[i]]) <- retro_cases[j,]
                dimnames(aij[[i]])=list(names, names)
                plot(aij[[i]], 
                     file = paste0("cms-3/matrix(",i,j,").dot"))
        }
}

generate_code_latex <- function(){
        fileConn <-file("code.tex")
        c <- "/home/josue/ICE/"
        code <- c()
        k <- 0
        for (i in 1:rowa_aij){
                for (j in 1:rows_rc){
                        k <- k + 1
                        bloque <-bloque_image(paste0(c,"cms-3/matrix(",i,j,").dot"))
                        if (k %% 4 != 0){
                                bloque <- c(bloque, separator())
                        }else{
                                bloque <- c(bloque, newline())
                        }
                        
                        code <- c(code, bloque)                       
                }
        }
        return(code)
}



writeLines(generate_code_latex(),"code.tex")
casos = permutations(n = 3, r=3, v =c("x","y","z"))

