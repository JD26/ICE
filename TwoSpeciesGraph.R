library(gtools)
source('libraryICE.R')

Interac.2.species <- list( Mutualism = c(1,1), 
                           Predator.Prey = c(-1,1), 
                           Competition = c(-1,-1))
#Tambipen podemos ver que se puede expresar como combinación.
#inter_cases = combinations(n = 2, r = 2, v = c(1,-1), 
#                           repeats.allowed =TRUE)

#Las retroacciones se pueden expresar como permutaciones
Retroac.2.species <- permutations(n = 3, r = 2, 
                                  v = c(0,1,-1), 
                                  repeats.allowed =TRUE)

if ( !file.exists("inter_2")){
        dir.create("inter_2")
}

if ( !file.exists("inter_retro_2")){
        dir.create("inter_retro_2")
}

i <- 0
inter_name <- names(Interac.2.species)
names_files <- paste0(inter_name,".dot")
for ( interaction in Interac.2.species ){
        i <- i + 1
        name_file <- names_files[i]
        writeLines(get_code_dot_4(interaction),paste0("inter_2/",name_file))        
}

writeLines(generate_code_latex("/home/josue/ICE/inter_2/",names_files),"inter_2/code.tex")

i <- 0
names_files <- c()
for ( interaction in Interac.2.species ){
        i <- i + 1
        for (j in 1:nrow(Retroac.2.species)){
                name_file <- paste0(inter_name[i],
                                    "(", Retroac.2.species[j,1],
                                    ",", Retroac.2.species[j,2], ").dot")
                names_files <- c(names_files, name_file)
                writeLines(get_code_dot_5(Retroac.2.species[j,],interaction), 
                           paste0("inter_retro_2/",name_file))            
        } 
}

writeLines(generate_code_latex("/home/josue/ICE/inter_retro_2/",names_files),"inter_retro_2/code.tex")

print(nrow(Retroac.2.species)*3)






