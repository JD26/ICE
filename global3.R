library(gtools)
#Intentamos encontrar todas las conexiones globales
#de 3 especies invariantes en rotacion y dirección.
#Encontramos 13. 10 resultan de las combinaciones y las tres
#restantes las agregamos. Las descubrimos con papel y lapiz.
source('libraryICE.R')

names <- c("x","y","z")
#Funciones que verifican si dos secuencias son iguales
test_1 <- function(seq_orig, i, seq_test){
        identical(c( seq_orig[i:6], seq_orig[-i:-6] ),
                  seq_test)
}

test_2 <- function(seq_orig, i, seq_test){
        i <- 7 - i 
        identical(c( seq_orig[6:1][i:6], seq_orig[6:1][-i:-6]),
                  seq_test)
}

build_matriz <- function(x,y,z){
        matrix(c(0,y[1],z[1],
                 x[1],0,z[2],
                 x[2],y[2],0),
               byrow = TRUE, nrow = 3)
}

validate_seq <- function(seq_orig, seq_test){
        #lenngth(seq_test) = 6
        for ( i in c(1,3,5) ){
                if ( seq_orig[i] == seq_test[1] ){
                        if (test_1(seq_orig, i, seq_test)){
                                return(TRUE)
                        }
                }
        }
        
        for ( i in c(2,4,6) ){
                if ( seq_orig[i] == seq_test[1] ){
                        if (test_2(seq_orig, i, seq_test)){
                                return(TRUE)
                        }
                }
        }
        return(FALSE)
}

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

global_cases <- combinations(n = 3, r=3, v =c("a","b","c"),
                             repeats.allowed =TRUE)

nr <- nrow(global_cases)
orig_vector <- c(chave_gc(global_cases[5,1]),
                 chave_gc(global_cases[5,2]),
                 chave_gc(global_cases[5,3]))

result <- c()
#Lista que contiene los valores de las conexiones.
lista_aij <- list()
for (i in 1:nr){
        test_vector <- c(chave_gc(global_cases[i,1]),
                         chave_gc(global_cases[i,2]),
                         chave_gc(global_cases[i,3]))
        #print(test_vector)
        lista_aij <- c(lista_aij, list(test_vector))
        test <- validate_seq(orig_vector,test_vector)
        if (test){
                #print(test_vector)
        }
        result <- c(result, test)
}
print(orig_vector)
summary(result)

#Por ultimo agregamos a la lista los casos no considerados:
lista_aij <- c(lista_aij, list( c(1, 1, -1, -1, -1, 1) ))
lista_aij <- c(lista_aij, list( c(1, -1, -1, 1, 1, 1) ))
lista_aij <- c(lista_aij, list( c(1, -1, -1, 1, -1, -1) ))


for ( i in 1:length(lista_aij) ){
        result <- c()
        for ( j in 1:length(lista_aij) ){
                test <- validate_seq( lista_aij[[i]], 
                                      lista_aij[[j]] )
                result <- c(result, test)
        }
        print(summary(result))
}
print("Resultados: Ninguna de las combinaciones es igual a 
      la otra excepto a si misma.")

#verifica que la carpeta existe
if ( !file.exists("global_3")){
        dir.create("global_3")
}

#Convierte las matrices en archivos .dot
names_files <- c()
for (i in 1:length(lista_aij)){
        v <- lista_aij[[i]]
        name_file <- paste0('matrix(',i,').dot')
        names_files <- c(names_files, name_file)
        writeLines(get_code_dot(v),paste0("global_3/",name_file))
}

writeLines(generate_code_latex("/home/josue/ICE/global_3/",names_files),"global_3/code.tex")

#Ahora veamos las retroacciones posbles
retro_cases <- permutations(n = 3, r = 3, v = c(0,1,-1), 
                            repeats.allowed =TRUE)

if ( !file.exists("global_retro_3")){
        dir.create("global_retro_3")
}

names_files <- c()
for (i in 1:length(lista_aij)){
        for (j in 1:nrow(retro_cases)){
                name_file <- paste0("global_retro_3/matrix(",i,j,").dot")
                names_files <- c(names_files, name_file)
                writeLines(get_code_dot_1(retro_cases[j,],
                                          lista_aij[[i]]),
                           name_file)
        }
}

writeLines(generate_code_latex("/home/josue/ICE/global_retro_3/",names_files),"global_retro_3/code.tex")
N <- length(lista_aij)*nrow(retro_cases)
print(paste0("Se generaron ",N," archivos(casos) de retroacciones globales."))
