source('libraryICE.R')
ubication.file.G <- "/home/josue/ICE/Global_Interaction_3"
ubication.file.NG <- "/home/josue/ICE/No_Global_Interaction_3"
if ( !file.exists(ubication.file.G) ){
        dir.create(ubication.file.G)
}
if ( !file.exists(ubication.file.NG) ){
        dir.create(ubication.file.NG)
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
                Predation.Mutualism.1 = list(desc = "Predador en Mutualismo",
                                             inter = c(-1, 1, 1, 1)),
                Double.Predation = list(desc = "Predadores-Presa",
                                        inter = c(1, -1, -1, 1)),
                Competition.Mutualism = list(desc = "Competencia y Mutualismo",
                                             inter = c(-1, -1, 1, 1)),
                Predation.Mutualism.2 = list(desc = "Presa en Mutualismo",
                                             inter = c(1, -1, 1, 1)),
                Total.Mutualism = list(desc = "Mutualismo Total",
                                       inter = c(1, 1, 1, 1))
                ) 
}
Global_Interaction_3 <- function(){
        conection_global_3 <- list(
                Total.Predation = list(group = 1, 
                                       desc = "Predador-Presa Total",
                                       inter = c(-1, 1, -1, 1, -1, 1)),
                TopPredator.SubPredation = list(group = 1,
                                                desc = "Predador Top y Sub-PP",
                                                inter = c(-1, 1, 1, -1, -1, 1)),
                Total.Mutualism = list(group = 2,
                                       desc = "Mutualismo Total",
                                       inter = c(1, 1, 1, 1, 1, 1)),
                Mutualism.SubPredation = list(group = 2,
                                              desc = "Mutualismo y Sub-PP",
                                              inter = c(1, 1, 1, 1, -1, 1)),
                TopPredator.SubMutualism = list(group = 2,
                                                desc = "Predador Top y Sub-Mu.",
                                                inter = c(-1, 1, 1, -1, 1, 1)),
                PredatorPrey.SubMutualism = list(group = 2,
                                                 desc = "Predador-Presa y Sub-Mu.",
                                                 inter = c(-1, 1, -1, 1, 1, 1)),
                Prey.SubMutualism = list(group = 2,
                                         desc = "Mutualismo Predador",
                                         inter = c(1, -1, -1, 1, 1, 1)),
                Total.Competition = list(group = 3,
                                         desc = "Competencia Total",
                                         inter = c(-1, -1, -1, -1, -1, -1)),
                Competition.SubMutualism = list(group = 3,
                                                desc = "Competencia y Sub-Mu.",
                                                inter = c(-1, -1, -1, -1, 1, 1)),
                Competition.SubPreadation = list(group = 3,
                                                 desc = "Competencia y Sub-PP",
                                                 inter = c(-1, -1, -1, -1, -1, 1)),
                Competition.PP.Mutualism1 = list(group = 3,
                                                 desc = "Competencia-Mu-PP A",
                                                 inter = c(-1, -1, 1, 1, -1, 1)),
                Competition.PP.Mutualism2 = list(group = 3,
                                                 desc = "Competencia-Mu-PP B",
                                                 inter = c(-1, -1, 1, 1, 1, -1)),
                SubCompetence.Mutualismo = list(group = 3,
                                                desc = "Mutualismo y Sub-Co.",
                                                inter = c(1, 1, 1, 1, -1, -1)),
                PredatorPrey.SubCompetition = list(group = 3,
                                                   desc = "Predador-Presa y Sub-Co.",
                                                   inter = c(-1, 1, -1, 1, -1, -1)),
                TopPredator.SubCompetition = list(group = 3,
                                                  desc = "Predador Top y Sub-Co.",
                                                  inter = c(-1, 1, 1, -1, -1, -1)),
                Pres.SubCompetition = list(group = 3,
                                           desc = "Competencia Predador",
                                           inter = c(1, -1, -1, 1, -1, -1))
        )
}

WriteCodeDot_NG <- function(Interactions, ubication_file){
        names_files <- paste0(gsub("(\\.)"," ",names(Interactions)),".dot")
        for (i in 1:length(Interactions)){
                writeLines(get_code_dot_2(Interactions[[i]]$inter, 
                                          label = as.character(Interactions[[i]]$desc)),
                           paste0(ubication_file,"/",names_files[i]))
        }
        names_files
}
WriteCodeDot_G <- function(Interactions, ubication_file){
        names_files <- paste0(gsub("(\\.)"," ",names(Interactions)),".dot")
        for (i in 1:length(Interactions)){
                writeLines(get_code_dot(Interactions[[i]]$inter, 
                                        label = as.character(Interactions[[i]]$desc)),
                           paste0(ubication_file,"/",names_files[i]))
        }
        names_files
}



Interactions <- No_Global_Interacction_3()
length(Interactions)
names_files <- WriteCodeDot_NG(Interactions, ubication.file.NG)

writeLines(generate_code_latex(ubication.file.NG,
                               names_files,
                               step = 2,
                               sep = "quad",
                               interline = TRUE),
           paste0(ubication.file.NG,"/code.tex"))

Interactions <- Global_Interaction_3()
length(Interactions)
names_files <- WriteCodeDot_G(Interactions, ubication.file.G)

writeLines(generate_code_latex(ubication.file.G,
                               names_files,
                               step = 3,
                               sep = "quad",
                               interline = TRUE),
           paste0(ubication.file.G,"/code.tex"))

WriteHijoLyx(ubication.file.NG,
             "Interacciones no globales sin retroacciones entre tres especies.")

WriteHijoLyx(ubication.file.G,
             "Interacciones globales sin retroacciones entre tres especies.")
