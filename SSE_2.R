library(gtools)
source('libraryICE.R')
source('sign_stable.R')
#DESCRIPCION
# INTERACCIONES DE DOS ESPECIES
# SIGNO SEMI ESTABLES
ubication.file <- "/home/josue/ICE/SSE_2"
if ( !file.exists(ubication.file) ){
        dir.create(ubication.file)
}
Interac.2.species <- list( Mutualism = c(1, 1), 
                           Predator.Prey = c(-1, 1),
                           Competition = c(-1, -1),
                           Amensalismo = c(-1, 0),
                           Comensalismo = c(0, 1),
                           Neutralismo = c(0, 0)
)


Retroac.2.species <- permutations(n = 2, r = 2, 
                                  v = c(0,-1,1), 
                                  repeats.allowed =TRUE)

matrix_se <- list()
for ( interaction in Interac.2.species ){
        for (j in 1:nrow(Retroac.2.species)){                
                v <- c(Retroac.2.species[j,1],
                       interaction[1],
                       interaction[2],
                       Retroac.2.species[j,2])
                M <- matrix(v,nrow = 2)
                test <- test_sign_semi_stable(M)                
                if (test ){
                        matrix_se <- c(matrix_se,list(M))
                        print(M)
                }
        } 
}

names <- 1:length(matrix_se)
i <- 0
for ( m in matrix_se){
        i <- i+1
        writeLines(get_code_dot_5(diag(m),c(m[2,1],m[1,2])), 
                   paste0(ubication.file,"/",names[i])) 
}


writeLines(generate_code_latex(ubication.file,
                               names,
                               step = 3,
                               sep = "quad",
                               interline = TRUE),
           paste0(ubication.file,"/code.tex"))

WriteHijoLyx(ubication.file,
             "Interacciones Signo-Semi-Estables.")

