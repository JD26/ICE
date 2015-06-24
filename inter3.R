library(gtools)
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
        for (j in 1:rows_lc){
                inter <- c(lateral_cases[j,1],
                           central_cases[i,],
                           lateral_cases[j,2])                
                aij <- c(aij, list(inter))
        }
}

#Escribe las interacciones en archivos .dot
if ( !file.exists("inter_3")){
        dir.create("inter_3")
}
names_files <- c()
for ( i in 1:length(aij) ){
        name_file <- paste0("matrix(",i,").dot")
        names_files <- c(names_files, name_file)
        writeLines(get_code_dot_2(aij[[i]]),paste0("inter_3/",name_file))
}
writeLines(generate_code_latex("/home/josue/ICE/inter_3/",names_files),"inter_3/code.tex")

#Escribe las retroacciones e interacciones .dot
if ( !file.exists("inter_retro_3")){
        dir.create("inter_retro_3")
}
retro_cases <- permutations(n = 3, r = 3, v = c(0,1,-1), 
                           repeats.allowed =TRUE) 
names_files <- c()
for (i in 1:length(aij)){
        for (j in 1:nrow(retro_cases)){
                name_file <- paste0("matrix(",i,j,").dot")
                names_files <- c(names_files, name_file)
                writeLines(get_code_dot_3(retro_cases[j,], aij[[i]]),paste0("inter_retro_3/",name_file))
        }
}

writeLines(generate_code_latex("/home/josue/ICE/inter_retro_3/",names_files),"inter_retro_3/code.tex")



