source('libraryICE.R')
ubication.file <- "/home/josue/ICE/Global_Interaction_3"
Global_Interaction_3 <- function(){
        conextion_global_3 <- list(
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

if ( !file.exists(ubication.file) ){
        dir.create(ubication.file)
}

Interactions <- Global_Interaction_3()
names_files <- paste0(gsub("(\\.)"," ",names(Interactions)),".dot")
for (i in 1:length(Interactions)){
        writeLines(get_code_dot(Interactions[[i]]$inter, 
                                label = as.character(Interactions[[i]]$desc)),
                   paste0(ubication.file,"/",names_files[i]))
}

length(Interactions)

writeLines(generate_code_latex(ubication.file,
                               names_files,
                               step = 3,
                               sep = "quad",
                               interline = TRUE),
           paste0(ubication.file,"/code.tex"))

WriteHijoLyx(ubication.file,
             "Interacciones globales sin retroacciones entre tres especies.")
