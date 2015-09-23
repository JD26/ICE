rm(list=ls())
library(deSolve)
library(ggplot2)
library(reshape)
library(grid) 
library(extrafont)


xmin <- 0
xmax <- 5
ymin <- 0
ymax <- 1.3

# parameters

mu <- 0.2
beta <- 2
gamma <- 0.3
c <- 0.3
sigma <- 1
delta <- 0.2

T0 <- sigma/c
T1 <- 1-mu
T2 <- ((sigma*gamma*c + 2*beta*delta*sigma) - 2*sigma*sqrt(beta*delta*(beta*delta+c*gamma)))/(c*c*gamma)




# others
TX <-0.16; 
Delta <- ifelse(TX == T2, 0, ((sigma - TX*c)^2)*(gamma^2) - 4*gamma*TX*beta*delta*sigma)

F1 <- function(x,y) {
        max(0,1-TX)*x - mu*x - x*y
}

F2 <- function(x,y) {
        gamma*max(0,1-(TX*c/sigma)-(TX*beta/sigma)*x)*x*y-delta*y
        #gamma*(1-(TX*c/sigma)-(TX*beta/sigma)*x)*x*y-delta*y
}

DE <- function (t, y, parms) {
        with(as.list(y), {
                dx <- F1(x,y)
                dy <- F2(x,y)
                list(c(dx, dy))
        })
}

grilla <- function(xi,xf,yi,yf,paso) {
        x = seq(xi,xf,by=paso)
        y = seq(yi,yf,by=paso)
        nx = length(x)
        ny = length(y)
        datadx = c()
        datady = c()
        for(xi in x)
        {
                datadx = c(datadx,rep(xi,ny))
                datady = c(datady,y)  
        }
        data.frame(x=datadx, y=datady, dx=rep(0,nx*ny), dy=rep(0,nx*ny))
}

field_vector <- function(xi,xf,yi,yf,paso) {
        data = grilla(xi,xf,yi,yf,paso)
        n = dim(data)[1]
        for (i in 1:n){
                x = data$x[i]
                y = data$y[i]
                dx = F1(x,y)
                dy = F2(x,y)
                norma = sqrt( dx*dx + dy*dy )*30
                data$dx[i] = dx/norma
                data$dy[i] = dy/norma
        }
        data
}

solution <- function(xi, yi) {
        ic <- c(x = xi, y = yi)
        times <- seq(from = 100, to = 0, by = -0.05)
        aproxsol1 <- ode(y = ic, times = times, func = DE,parms = NULL)
        aproxsol1 = data.frame(aproxsol1)
        aproxsol1 <- aproxsol1[with(aproxsol1,order(time)),]
        colnames(aproxsol1) = c("time","prey","predator")
        times <- seq(from = 0, to = 100, by = 0.05)
        aproxsol2 <- ode(y = ic, times = times, func = DE,parms = NULL)
        aproxsol2 = data.frame(aproxsol2)
        colnames(aproxsol2) = c("time","prey","predator")
        rbind(aproxsol1,aproxsol2)
        
}

PlotVectorField <- function() {
        size_iso <- 1.4
        limite <-(1-TX*c/sigma)*(sigma/(TX*beta))
        umbral <- data.frame(x = c(limite,limite), y = c(0, ymax))
        isox_x <- data.frame(x = c(0, 0), y = c(0, ymax))
        isoy_y <- data.frame(x = c(0, xmax), y = c(0, 0))
        g <- ggplot()
        g <- g + geom_segment(data = field_vector(xmin,xmax,ymin,ymax, paso=0.1),
                              aes(x=x, y=y, xend=x + dx, yend= y +dy), 
                              arrow = arrow(length = unit(0.1,"cm")),
                              color = "#A0A0A0") 
        #g <- g + geom_path(data = umbral, aes(x, y), color = "#58ACFA",size = size_iso)
        #g <- g + geom_path(data = umbral, aes(x/2, y), color = "#58ACFA",size = size_iso)
        if (TX < T1) {
                x_y <- 1-TX-mu
                isox_y <- data.frame(x = c(0, xmax), y = c(x_y, x_y))
                #g <- g + geom_path(data = isox_y, aes(x, y),size = size_iso)
        }
        #g <- g + geom_path(data = isox_x, aes(x, y), size = size_iso)
        #g <- g + geom_path(data = isoy_y, aes(x, y), color = "#0000FF",size = size_iso)
        if ( Delta > 0 ) {
                y_x_1 <- ( (sigma - TX*c)*gamma - sqrt(Delta) )/(2*gamma*TX*beta)
                y_x_2 <- ( (sigma - TX*c)*gamma + sqrt(Delta) )/(2*gamma*TX*beta)
                isoy_x_1 <- data.frame(x = c(y_x_1, y_x_1), y = c(0, ymax))
                isoy_x_2 <- data.frame(x = c(y_x_2, y_x_2), y = c(0, ymax))
                #g <- g + geom_path(data = isoy_x_1, aes(x, y), color = "#0000FF",size = size_iso)
                #g <- g + geom_path(data = isoy_x_2, aes(x, y), color = "#0000FF",size = size_iso)
                g <- g + geom_point(data = data.frame(x=c(y_x_1),y = c(1-TX-mu)),aes(x,y), color = "red", shape = 16, size=4)
                g <- g + geom_point(data = data.frame(x=c(y_x_2),y = c(1-TX-mu)),aes(x,y), color = "red", shape = 16, size=4)                
        }        
        if ( Delta == 0 ) {
                y_x <- ((sigma - TX*c))/(2*TX*beta)
                isoy_x <- data.frame(x = c(y_x, y_x), y = c(0, ymax))
                #g <- g + geom_path(data = isoy_x, aes(x, y), color = "blue",size = 2)
                g <- g + geom_point(data = data.frame(x=c(y_x),y = c(1-TX-mu)),aes(x,y), color = "red", shape = 16, size=4)                
        }
        g <- g + geom_point(data = data.frame(x=c(0),y = c(0)),aes(x,y), color = "red", shape = 16, size=4)
        g <- g + xlim(xmin,xmax+0.01)
        g <- g + ylim(ymin,ymax+0.01)
        g <- g + labs(x = "x(presa)", y = "y(predador)")
        g <- g + theme_bw(base_size = 12, base_family = "Helvetica")
        g <- g + theme(panel.grid.major = element_line(size = 0.5, 
                                                       color = "grey"), 
                       axis.line = element_line(size = 0.7, 
                                                color = "black"), 
                       legend.position = c(0.85,0.7), 
                       text = element_text(size = 14,family="CM Roman"))                
        g
}

g <- PlotVectorField()
g

g <- g+ geom_path(data = solution(1,0.8) ,
                  aes(x = prey, y = predator)) 
g <- g+ geom_path(data = solution(1,0.7) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(1,0.6) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(1,0.5) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(1,0.4) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(1,0.3) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(1,0.2) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(1,0.1) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2,0.9) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2,0.8) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.5,0.95) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.5,0.8) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.5,0.9) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.5,1) ,
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(1.5,1-TX-mu),
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(1.6,1-TX-mu),
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(1.7,1-TX-mu),
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2,0.6),
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.5,0.6),
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.5,0.58),
                aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.5,0.55),
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.5,0.54),
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.7,0.715),
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.8,0.7),
                  aes(x = prey, y = predator))
g <- g+ geom_path(data = solution(2.8,0.65),
                  aes(x = prey, y = predator))
xpoint <- ( (sigma - TX*c)*gamma + sqrt(Delta) )/(2*gamma*TX*beta)
ypoint <- 1-TX-mu +0.01
g <- g+ geom_path(data = solution(xpoint,ypoint),
                  aes(x = prey, y = predator),color="green")
g

ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="fase015(T0.8).pdf")

PlotTimeVsBiomass <- function(xi,yi,Tf) {        
        ic <- c(x = xi, y = yi)
        times <- seq(from = 0, to = Tf, by = 0.05)
        df <- ode(y = ic, times = times, func = DE,parms = NULL)
        df = data.frame(df)
        colnames(df) = c("time","x(presa)","y(predador)")        
        df <- melt(df, id=c("time"))
        colnames(df) = c("time","Población","biomass")
        g <- ggplot()
        g <- g + geom_path(data = df , aes(x = time, y = biomass, linetype = Población))
        g <- g + labs(x = "t (tiempo)", y = "Densidad de población")
        g <- g + theme_bw(base_size = 12, base_family = "Helvetica")
        g <- g + theme(panel.grid.major = element_line(size = 0.5, 
                                                       color = "grey"), 
                       axis.line = element_line(size = 0.7, 
                                                color = "black"), 
                       legend.position = c(0.85,0.7), 
                       text = element_text(size = 14,family="CM Roman"))                
        g
}

#TX=0.9
g <- PlotTimeVsBiomass(1.5,0.9,20)
ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="VsTime(T0.9).pdf")
#TX=T1
g <- PlotTimeVsBiomass(0.8,0.4,50)
ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="VsTime(TT1).pdf")
#TX=0.5
g <- PlotTimeVsBiomass(1.5,0.9,20)
ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="VsTime(T0.5)1.pdf")
g <- PlotTimeVsBiomass(0.5,0.2,10)
ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="VsTime(T0.5)2.pdf")
#TX=T2
g <- PlotTimeVsBiomass(0.8,0.6,10)
ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="VsTime(TT2)1.pdf")
g <- PlotTimeVsBiomass(1.5,0.9,20)
ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="VsTime(TT2)2.pdf")
#TX=0.14
g <- PlotTimeVsBiomass(0.8,0.7,100)
ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="VsTime(T014)1.pdf")
g <- PlotTimeVsBiomass(0.8,0.9,20)
ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="VsTime(T014)2.pdf")
g <- PlotTimeVsBiomass(2.5,0.65,10)
ggsave("ggplot_cm.pdf", g, width=5, height=5)
embed_fonts("ggplot_cm.pdf", outfile="VsTime(T014)3.pdf")





