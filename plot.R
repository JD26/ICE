plot <- function (CM, file = stop("'file' must be specified"), color = "bw") 
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