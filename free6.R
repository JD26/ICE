library(gtools)

diagonal1 <- combinations(n = 3, r = 2, 
                  v = c(0,1,-1), 
                  repeats.allowed =TRUE)
diagonal2 <- permutations(n = 3, r = 2, 
                          v = c(0,1,-1), 
                          repeats.allowed =TRUE)

otros <- combinations(n = 2, r = 2, 
                  v = c(1,-1), 
                  repeats.allowed =TRUE)
n <- 2*nrow(diagonal1)+ nrow(diagonal2)
print(n)

for (i in 2:4){
        d <- permutations(n = 3, r = i**2, 
                                  v = c(0,1,-1), 
                                  repeats.allowed =TRUE)
        print(nrow(d))
}
