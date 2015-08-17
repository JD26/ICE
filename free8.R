library(gtools)
source('libraryICE.R')
source('sign_stable.R')
#DESCRIPCION
# INTERACCIONES DE TRES ESPECIES
# SIGNO ESTABLES
ubication.file <- "/home/josue/ICE/SE_3"
if ( !file.exists(ubication.file) ){
        dir.create(ubication.file)
}

No_Global_Interacction_3 <- function(){
        conection_3 <- list(
                Total.Competition = list(desc = "Competencia Total",
                                         inter = c(-1, -1, -1, -1)),
                Predation.Competition.1 = list(desc = "Presa en Competencia",
                                               inter = c(1, -1, -1, -1)),
                Predation.Competition.2 = list(desc = "Predador en Competencia",
                                               inter = c(-1, -1, 1, -1)),
                Total.Predation = list(desc = "Predación Total",
                                       inter = c(1, -1, 1, -1)),
                Top.Predator = list(desc = "Predador Top",
                                    inter = c(-1, 1, 1, -1)),               
                Double.Predation = list(desc = "Predadores-Presa",
                                        inter = c(1, -1, -1, 1))                               
        ) 
}