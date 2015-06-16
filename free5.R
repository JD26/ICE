library(gtools)
library(LoopAnalyst)
source('plot.R')
source('libraryICE.R')

#Caso que incluye todas las interacciones menos las retroacciones
#para 3 especies.
names <- c("x","y","z")
global_cases <- combinations(n = 4, r=3, v =c("a","b","c","d"),repeats.allowed =TRUE)
chave_gc <- function(letter){
        if (letter == "a"){
                return(c(-1,-1))
        }
        if (letter == "b"){
                return(c(1,1))
        }
        if (letter == "c"){
                return(c(-1,1))
        }
        if (letter == "d"){
                return(c(1,-1))
        }
}
build_matriz <- function(x,y,z){
        matrix(c(0,y[1],z[1],
                 x[1],0,z[2],
                 x[2],y[2],0),
               byrow = TRUE, nrow = 3)
}
nr <- nrow(global_cases)
#Crea los graficos en formato .dot
for (i in 1:nr){
        x_efect <- chave_gc(global_cases[i,1])
        y_efect <- chave_gc(global_cases[i,2])
        z_efect <- chave_gc(global_cases[i,3])
        mx = build_matriz(x_efect,
                          y_efect,
                          z_efect)
        dimnames(mx)=list(names, names)
        #print(mx)
        plot(mx, 
             file = paste0("cms-3-global/matrix(",i,").dot"))
}

generate_code_latex <- function(){
        c <- "/home/josue/ICE/"
        code <- c()
        k <- 0
        for (i in 1:nr){
                k <- k + 1
                bloque <- bloque_image(paste0(c,"cms-3-global/matrix(",i,").dot"))
                if (k %% 4 != 0){
                        bloque <- c(bloque, separator())
                }else{
                        bloque <- c(bloque, newline())
                }                
                code <- c(code, bloque)                                       
        }
        return(code)
}



writeLines(generate_code_latex(),"cms-3-global/code.tex")

