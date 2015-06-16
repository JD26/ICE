separator <- function(){
        return(c("\\begin_inset Formula $\\;$","\\end_inset"))
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
