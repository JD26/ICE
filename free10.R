library(gtools)
source('libraryICE.R')

#DESCRIPCION
# INTERACCIONES positiva y negativa
ubication.file <- "/home/josue/ICE"
if ( !file.exists(ubication.file) ){
        dir.create(ubication.file)
}
Interac.2.species <- list( Positivo = c(0, 1), 
                           Negativo = c(0, -1)
)

i <- 0
inter_name <- names(Interac.2.species)
names_files <- paste0(inter_name,".dot")
for ( interaction in Interac.2.species ){
        i <- i + 1
        name_file <- names_files[i]
        writeLines(get_code_dot_4(interaction),paste0(ubication.file,"/",name_file))
}
