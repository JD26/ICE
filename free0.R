library(ggplot2)
library(reshape2)
##Tema
science_theme = theme(panel.grid.major = element_line(size = 0.5, 
                                                      color = "grey"), 
                      axis.line = element_line(size = 0.7, 
                                               color = "black"), 
                      #legend.position=c(1,1), 
                      #legend.justification=c(1,1), 
                      text = element_text(size = 14),
                      axis.ticks=element_blank(),
                      axis.text=element_blank())+
        theme_bw(base_size = 12, base_family = "Helvetica")

f_original <- function(x, alfa1 = 1, alfa2 = 1, l = 1){
        alfa1*(max(0, 1-alfa2*x))^l
}

f_exp <- function(x, alfa1 = 1, alfa2 = 1){
        alfa1*exp(-alfa2*x)
}

f_exp_2 <- function(x, alfa1 = 1, alfa2 = 1){
        alfa1*exp(-alfa2*x^2)
}

f_logistica_modi <- function(x, alfa1 = 1, alfa2 = 1){
        2*alfa1/( 1 + exp(alfa2*x) ) 
}

x <- seq(0,1,0.01)
y1 <- 1-x
y2 <- sqrt(1-x)
y3 <- (1-x)^2

d = data.frame(x, y1, y2, y3)
g <- ggplot()+
        geom_path(data = d, aes(x,y1),linetype = 2)+
        geom_path(data = d, aes(x,y2),linetype = 3)+
        geom_path(data = d, aes(x,y3))+
        science_theme        
g

x <- seq(0,5, by = 0.01)
y1.1 <- sapply(x, f_original)
y1.2 <- sapply(x, f_original, l = 4)
y2 <- sapply(x, f_exp)
y3 <- sapply(x, f_exp_2)
y4 <- sapply(x, f_logistica_modi)

d = data.frame(x,y1.1,y1.2,y2,y3,y4)
d = melt(d, id.vars = c("x"))
g <- ggplot()+
        geom_path(data = d, aes(x = x, 
                                y = value, 
                                group = variable, 
                                linetype =variable))+
        science_theme+
        labs(linetype="")+
        ylab(expression(symbol(rho)(y)))+
        xlab("y")+
        scale_linetype_discrete(labels=c("Tipo 1(a)",
                                       "Tipo 1(b)",
                                       "Tipo 2",
                                       "Tipo 3",
                                       "Tipo 4"))        
g
















set.seed(2)
f1 <- function(x){
#         if (x < 1){
#                 return ((1-x)^0.5)
#         }else{
#                 return (0)
#         }
        exp(-x^2)
}

generator <- function(a, b, f){
        K <- c()
        points <- seq(a, b, by = 0.0001)
        for (i in 1:10000){
                v <- sample(points, 2)
                k <- (f(v[1]) - f(v[2]))/(v[1]-v[2])
                K <- c(K, abs(k))
        }        
        summary(K)
}

 
generator(0,10, f1)


