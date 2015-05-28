library(ggplot2)
##Tema
science_theme = theme(panel.grid.major = element_line(size = 0.5, 
                                                      color = "grey"), 
                      axis.line = element_line(size = 0.7, 
                                               color = "black"), 
                      legend.position = c(0.85,0.7), 
                      text = element_text(size = 14),
                      axis.ticks=element_blank(),
                      axis.text=element_blank())+
        theme_bw(base_size = 12, base_family = "Helvetica") 

##Función fi
fifunction <- function(y,ro){
        y*(max(0,1-y)+ro)
}

##Función parecida a sapply
fivalues <- function(ro){
        y <- seq(0,max(ro,0.5)+1,0.01)
        fi <- sapply(y, function(x) fifunction(x,ro) ) 
        data.frame(y, fi)
}

##Función que grafica
graphicfiy <- function(ro,file){
        g <- ggplot()+
                geom_path(data = fivalues(ro),aes(y,fi))+
                geom_path(data = data.frame(x = c(0,1), 
                                            y = c(ro,ro)),
                          aes(x,y),
                          linetype = 2)+
                geom_path(data = data.frame(x = c(ro,ro),
                                            y = c(0,fifunction(ro,ro))),
                          aes(x,y),
                          linetype = 3)+
                ylab(expression(symbol(phi)(y)))+
                xlab(expression(y))+
                science_theme
        ggsave(plot=g,file,width = 3,height = 3)       
}

##Grafica fi(y) con valores de ro diferentes
graphicfiy(1.3,"ThesisICE/images/chapter5/romore.pdf")
graphicfiy(0.4,"ThesisICE/images/chapter5/roless.pdf")

