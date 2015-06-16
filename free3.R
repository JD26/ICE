library(gtools)
library(LoopAnalyst)
source('plot.R')
#2 x-y
names <- c("x","y")
inter_cases = combinations(n = 2, r = 2, v = c(1,-1), 
                           repeats.allowed =TRUE)
n.inter_cases = dim(inter_cases)[1]
retro_cases = permutations(n = 3, r = 2, v = c(0,1,-1), 
                           repeats.allowed =TRUE)
n.retro_cases = dim(retro_cases)[1]
for (i in 1:n.inter_cases){
        a12 <- inter_cases[i,2]
        a21 <- inter_cases[i,1]
        for (j in 1:n.retro_cases){
                a11 <- retro_cases[j,1]
                a22 <- retro_cases[j,2]
                mx = matrix(c(a11, a12, a21, a22),
                            nrow = 2,
                            byrow = TRUE,
                            dimnames = list(names, names))
                plot(mx, 
                     file = paste0("cms-2/matrix(",
                                   a11,",",a12,",",a21,",",a22,").dot"))
        }
}





