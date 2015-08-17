generate_code_latex <- function(root, names, step = 4, sep = ";", interline = FALSE){
        n <- length(names)
        code <- c()
        k <- 0
        for (i in 1:n){
                k <- k + 1
                bloque <- bloque_image(paste0(root,"/",names[i]))
                if (k %% step != 0){
                        bloque <- c(bloque, separator(sep = sep))
                }else{
                        bloque <- c(bloque, 
                                    newline(),
                                    interline(),
                                    newline()
                                    )
                }                
                code <- c(code, bloque)                                       
        }
        return(code)
}

interline <- function(){
        c("\\begin_inset Formula $\\,$\\end_inset")
}

separator <- function(sep = ";"){
        return(paste0("\\begin_inset Formula $\\",sep,"$","\\end_inset"))
}


bloque_image <- function(file){
        c <- c("\n\\begin_inset Graphics",
                    paste("\t\tfilename",file),
                    "\t\tlyxscale 10",
                    "\t\twidth 4cm",
                    "\t\tgroupId two-species-inter-plot",
                    "\\end_inset")
        return(c)
}

newline <- function(){
        c("\\begin_inset Newline newline",
        "\\end_inset")
}


get_arrow <- function(inter){
        if ( identical(inter, c(1, 1)) ){
                return("")
        }
        if ( identical(inter, c(-1, -1)) ){
                return(", arrowtail=odot , arrowhead=odot")
        }
        if ( identical(inter, c(-1, 1)) ){
                return(", arrowtail=odot")
        }
        if ( identical(inter, c(1, -1)) ){
                return(", arrowhead=odot")
        }
        if ( identical(inter, c(-1, 0)) ){
                return(", arrowtail=odot, arrowhead=none")
        }
        if ( identical(inter, c(0, -1)) ){
                return(", arrowhead=odot, arrowtail=none")
        }
        if ( identical(inter, c(1, 0)) ){
                return(", arrowhead= none")
        }
        if ( identical(inter, c(0, 1)) ){
                return(", arrowtail= none")
        }
        if ( identical(inter, c(0, 0)) ){
                return(", arrowtail= none, arrowhead=none")
        }
}

get_arrow_retro <- function(var,retro){
        if ( retro == -1){
                return( paste0(var,' -> ',var,' [color="", arrowhead=odot ];') )
        }
        if ( retro == 0){
                return( "" )
        }
        if ( retro == 1){
                return( paste0(var,' -> ',var,' [color="" ];') )
        }
}

get_code_dot <- function(interactions, nodesep = 3, label =""){
        setences <- c("digraph G {",
                      paste0('graph [bgcolor = "transparent", size = "18!,18!", nodesep="',nodesep,'", ranksep="1", rankdir="LR"];'),
                      'node [fixedsize=true, fontname="Sans", fontsize="76.85", shape=circle, height="2", width="2", style="setlinewidth(4)"];',
                      'edge [style="setlinewidth(3)", arrowsize=3];',
                      '\t\tx [color=""];',
                      paste0('\t\t\t\tx -> y [color="", dir=both', get_arrow(interactions[1:2]),'];'),
                      '\t\ty [color=""];',
                      paste0('\t\t\t\ty -> z [color="", dir=both', get_arrow(interactions[3:4]),'];'),
                      '\t\tz [color=""];',
                      paste0('\t\t\t\tz -> x [color="", dir=both', get_arrow(interactions[5:6]),'];'),
                      ifelse( label == "", "", paste0('labelloc="b";\nlabel="',label,'";\nfontsize=40;') ),
                      '}' )
}

get_code_dot_1 <- function(retroactions, interactions, ranksep = 1.5){
        setences <- c("digraph G {",
                      paste0('graph [bgcolor = "transparent", size = "18!,18!", nodesep="1", ranksep="',ranksep,'", rankdir="LR"];'),
                      'node [fixedsize=true, fontname="Sans", fontsize="76.85", shape=circle, height="2", width="2", style="setlinewidth(4)"];',
                      'edge [style="setlinewidth(3)", arrowsize=3];',
                      '\t\tx [color=""];',
                      paste0("\t\t\t\t", get_arrow_retro("x",retroactions[1]) ),
                      paste0('\t\t\t\tx -> y [color="", dir=both', get_arrow(interactions[1:2]),'];'),
                      '\t\ty [color=""];',
                      paste0("\t\t\t\t", get_arrow_retro("y",retroactions[2]) ),
                      paste0('\t\t\t\ty -> z [color="", dir=both', get_arrow(interactions[3:4]),'];'),
                      '\t\tz [color=""];',
                      paste0("\t\t\t\t", get_arrow_retro("z",retroactions[3]) ),
                      paste0('\t\t\t\tz -> x [color="", dir=both', get_arrow(interactions[5:6]),'];'),
                      '}' )
}

get_code_dot_2 <- function(interactions, ranksep = 1.5, label =""){
        setences <- c("digraph G {",
                      paste0('graph [bgcolor = "transparent", size = "18!,18!", nodesep="1", ranksep="',ranksep,'", rankdir="LR"];'),
                      'node [fixedsize=true, fontname="Sans", fontsize="76.85", shape=circle, height="2", width="2", style="setlinewidth(4)"];',
                      'edge [style="setlinewidth(3)", arrowsize=3];',
                      '\t\tx [color=""];',
                      paste0('\t\t\t\tx -> y [color="", dir=both', get_arrow(interactions[1:2]),'];'),
                      '\t\ty [color=""];',
                      paste0('\t\t\t\ty -> z [color="", dir=both', get_arrow(interactions[3:4]),'];'),
                      ifelse( label == "", "", paste0('labelloc="b";\nlabel="',label,'";\nfontsize=40;') ),
                      '}' )
}

get_code_dot_3 <- function(retroactions, interactions, ranksep = 1.5){
        setences <- c("digraph G {",
                      paste0('graph [bgcolor = "transparent", size = "18!,18!", nodesep="1", ranksep="',ranksep,'", rankdir="LR"];'),
                      'node [fixedsize=true, fontname="Sans", fontsize="76.85", shape=circle, height="2", width="2", style="setlinewidth(4)"];',
                      'edge [style="setlinewidth(3)", arrowsize=3];',
                      '\t\tx [color=""];',
                      paste0("\t\t\t\t", get_arrow_retro("x",retroactions[1]) ),
                      paste0('\t\t\t\tx -> y [color="", dir=both', get_arrow(interactions[1:2]),'];'),
                      '\t\ty [color=""];',
                      paste0("\t\t\t\t", get_arrow_retro("y",retroactions[2]) ),
                      paste0('\t\t\t\ty -> z [color="", dir=both', get_arrow(interactions[3:4]),'];'),
                      '\t\tz [color=""];',
                      paste0("\t\t\t\t", get_arrow_retro("z",retroactions[3]) ),
                      '}' )
}

get_code_dot_4 <- function(interactions, ranksep = 2){
        setences <- c("digraph G {",
                      paste0('graph [bgcolor = "transparent", size = "18!,18!", nodesep="1", ranksep="',ranksep,'", rankdir="LR"];'),
                      'node [fixedsize=true, fontname="Sans", fontsize="76.85", shape=circle, height="2", width="2", style="setlinewidth(4)"];',
                      'edge [style="setlinewidth(3)", arrowsize=3];',
                      '\t\tx [color=""];',
                      paste0('\t\t\t\tx -> y [color="", dir=both', get_arrow(interactions),'];'),
                      '}' )
}

get_code_dot_5 <- function(retroactions, interactions, ranksep = 2){
        setences <- c("digraph G {",
                      paste0('graph [bgcolor = "transparent", size = "18!,18!", nodesep="1", ranksep="',ranksep,'", rankdir="LR"];'),
                      'node [fixedsize=true, fontname="Sans", fontsize="76.85", shape=circle, height="2", width="2", style="setlinewidth(4)"];',
                      'edge [style="setlinewidth(3)", arrowsize=3];',
                      '\t\tx [color=""];',
                      paste0("\t\t\t\t", get_arrow_retro("x",retroactions[1]) ),
                      paste0('\t\t\t\tx -> y [color="", dir=both', get_arrow(interactions),'];'),
                      '\t\ty [color=""];',
                      paste0("\t\t\t\t", get_arrow_retro("y",retroactions[2]) ),                      
                      '}' )
}


WriteHijoLyx <- function(ubication, texto = "", file = "code.tex"){
        v1 <- readLines("/home/josue/ICE/base/ImageLyx1.dat")
        v2 <- readLines(paste0(ubication,"/code.tex"))
        v3 <- c("\\end_layout",
                "\\begin_layout Plain Layout",
                "\\begin_inset Caption Standard",
                "\\begin_layout Plain Layout")
        v4 <- c("\\end_layout",
                "\\end_inset",
                "\\end_layout",
                "\\begin_layout Plain Layout",
                "\\end_layout",
                "\\end_inset",
                "\\end_layout",
                "\\end_body",
                "\\end_document")
        v5 <- c(v1, v2, v3, texto, v4)
        writeLines(v5,paste0(ubication,"/","code.lyx"))
}
plot2 <- function (CM, file = stop("'file' must be specified"), color = "bw") 
{
        validate.cm.names <- function(CM) {
                if (identical(rownames(CM), NULL)) {
                        CM.Name.Val <- c(1)
                }
                if ("" %in% rownames(CM) | NA %in% rownames(CM)) {
                        CM.Name.Val <- c(2)
                }
                else {
                        CM.Name.Val <- c(3)
                }
                if (identical(colnames(CM), NULL)) {
                        CM.Name.Val <- c(CM.Name.Val, 1)
                }
                if ("" %in% colnames(CM) | NA %in% colnames(CM)) {
                        CM.Name.Val <- c(CM.Name.Val, 2)
                }
                else {
                        CM.Name.Val <- c(CM.Name.Val, 3)
                }
                if (identical(CM.Name.Val[1], 3) & identical(CM.Name.Val[2], 
                                                             3) & !identical(rownames(CM), colnames(CM))) {
                        warning("\nParameter names are different for rows and columns!\nUsing row names for parameter names.")
                }
                return(CM.Name.Val)
        }
        #validate.cm(CM)
        N <- nrow(CM)
        CM.Name.Val <- validate.cm.names(CM)
        if (color == "color") {
                Colors <- c("#8000BF", "#0000BF", "#00BF00", "#BFBF00", 
                            "#FF8000", "#BF0000", "#8000FF", "#0080FF", "#008080", 
                            "#BFBF80", "#BF8000", "#FF00FF", "#804080", "#004080", 
                            "#408040", "#FEFE00", "#FFBF00", "#804040", "#8080FF", 
                            "#000080", "#004000", "#808000", "#FF8080", "#FF0080", 
                            "#400040", "#404080", "#BFFF00", "#FFFFBF", "#FFBF80", 
                            "#FF0000")
                arrowhead <- "dot"
        }
        if (color == "greyscale") {
                Colors <- c()
                for (i in 1:(N)) {
                        chunk <- format.hexmode(i * (255/N + 3))
                        chunk <- paste("#", chunk, chunk, chunk, sep = "")
                        Colors <- c(Colors, chunk)
                }
                arrowhead <- "dot"
        }
        if (color == "bw") {
                Colors <- c()
                for (i in 0:(N - 1)) {
                        Colors <- c(Colors, "#000000", sep = "")
                }
                arrowhead <- "odot"
        }
        if (!identical(CM.Name.Val[1], 3) & !identical(CM.Name.Val[2], 
                                                       3)) {
                sink(file = file)
                file.CM <- cat("digraph G {\ngraph [bgcolor = \"transparent\", size = \"18!,18!\", nodesep=\"1\", ranksep=\"1\", rankdir=\"LR\"];\nnode [fixedsize=true, fontname=\"Sans\", fontsize=\"75\", shape=circle, height=\"2\", width=\"2\", style=\"setlinewidth(4)\"];\nedge [style=\"setlinewidth(3)\", arrowsize=3];\n", 
                               sep = "")
                for (j in 1:N) {
                        file.CM <- cat("\t P", j, " [color=\"", Colors[j], 
                                       "\"];\n", sep = "")
                        file.CM <- cat("\t P", j, " [shape = circle];\n", 
                                       sep = "")
                        for (i in 1:N) {
                                if (CM[i, j] != 0) {
                                        file.CM <- cat(file.CM, "\t\t P", j, " -> P", 
                                                       i, sep = "")
                                        file.CM <- cat(file.CM, " [color=\"", Colors[j], 
                                                       "\"")
                                        if (CM[i, j] == -1) {
                                                file.CM <- cat(file.CM, ", arrowhead=", arrowhead, 
                                                               sep = "")
                                        }
                                        file.CM <- cat(file.CM, "];\n", sep = "")
                                }
                        }
                }
                file.CM <- cat(file.CM, "}", sep = "")
                sink()
        }
        if ((identical(CM.Name.Val[1], 3) | identical(CM.Name.Val[2], 
                                                      3))) {
                if (identical(CM.Name.Val[1], 3)) {
                        Parameters <- rownames(CM)
                }
                else {
                        Parameters <- colnames(CM)
                }
                max.name.len = 1
                for (j in 0:length(Parameters)) {
                        if (length(Parameters[j]) > max.name.len) {
                                max.name.len = length(Parameters[j])
                        }
                }
                sink(file = file)
                file.CM <- cat("digraph G {\ngraph [bgcolor = \"transparent\", size = \"18!,18!\", nodesep=\"1\", ranksep=\"1\", rankdir=\"LR\"];\nnode [fixedsize=true, fontname=\"Sans\", fontsize=\"", 
                               ((60/max.name.len) + (18 - (1.15^max.name.len))), 
                               "\", shape=circle, height=\"2\", width=\"2\", style=\"setlinewidth(4)\"];\nedge [style=\"setlinewidth(3)\", arrowsize=3];\n", 
                               sep = "")
                for (j in 1:N) {
                        file.CM <- cat("\t", Parameters[j], " [color=\"", 
                                       Colors[j], "\"];\n", sep = "")
                        for (i in 1:N) {
                                if (CM[i, j] != 0) {
                                        file.CM <- cat(file.CM, "\t\t", Parameters[j], 
                                                       " -> ", Parameters[i], sep = "")
                                        file.CM <- cat(file.CM, " [color=\"", Colors[j], 
                                                       "\"", sep = "")
                                        if (CM[i, j] == -1) {
                                                file.CM <- cat(file.CM, ", arrowhead=", arrowhead, 
                                                               sep = "")
                                        }
                                        file.CM <- cat(file.CM, "];\n", sep = "")
                                }
                        }
                }
                file.CM <- cat(file.CM, "}", sep = "")
                sink()
        }
}


#Ejemplo
#writeLines(get_code_dot(c(1,-1,-1,1,-1,-1)),"code.dot")
#writeLines(get_code_dot_1(c(1,0,-1),c(1,-1,-1,1,-1,-1)),"code.dot")