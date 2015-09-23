rm(list=ls())
library(deSolve)
library(ggplot2)
library(reshape)
library(grid) 
library(extrafont)

mu <- 0.2
beta <- 2
gamma <- 0.3
c <- 0.3
sigma <- 0.89
delta <- 0.2


T0 <- sigma/c
T1 <- 1-mu
T2 <- ((sigma*gamma*c + 2*beta*delta*sigma) - 2*sigma*sqrt(beta*delta*(beta*delta+c*gamma)))/(c*c*gamma)

TX <-0.9

# Singularmente Perturbado
F1 <- function(x,y,u,v) {
        max(0,1 - u)*x - mu*x - x*y
}
F2 <- function(x,y,u,v) {
        gamma*max(0,1 - v)*x*y - delta*y
}
G1 <- function(x,y,u,v,epsilon) {
        (TX - u - epsilon*max(0,1 - u)*u)/epsilon
}
G2 <- function(x,y,u,v,epsilon) {
        (c*TX - sigma*v - epsilon*gamma*max(0,1 - v)*x*v + beta*x*u)/epsilon
}
Model <- function (t, y, parms) {
        with(as.list(y), {
                dx <- F1(x,y,u,v)
                dy <- F2(x,y,u,v)
                du <- G1(x,y,u,v,parms[1])
                dv <- G2(x,y,u,v,parms[1])
                list(c(dx, dy, du, dv))
        })
}

# Reducido
RF1 <- function(x,y) {
        max(0,1-TX)*x - mu*x - x*y
}
RF2 <- function(x,y) {
        gamma*max(0,1-(TX*c/sigma)-(TX*beta/sigma)*x)*x*y-delta*y
}
RModel <- function (t, y, parms) {
        with(as.list(y), {
                dx <- RF1(x,y)
                dy <- RF2(x,y)                
                list(c(dx, dy))
        })
}

SolveModel <- function(ic,tf,eps) {
        times <- seq(from = 0, to = tf, by = 0.005)
        out <- ode(y = ic, times = times, func = Model,parms = c(eps))
        out <- data.frame(out)
        out
}

SolveRModel <- function(Ric,tf) {
        times <- seq(from = 0, to = tf, by = 0.005)
        Rout <- ode(y = Ric, times = times, func = RModel,parms = NULL)
        Rout <- data.frame(Rout)
        Rout$u <- rep(TX, (dim(Rout)[1])) 
        Rout$v <- TX*(c + beta*Rout$x)/sigma
        Rout
}

PlotforEps <- function(ic,tf,eps) {
        Ric <- ic[1:2]
        Rout <- SolveRModel(Ric,tf)
        gx <- ggplot()
        gy <- ggplot()
        gu <- ggplot()
        gv <- ggplot()          
        for (epsilon in eps) {        
                out <- SolveModel(ic,tf,epsilon)
                gx <- gx + geom_path(data = out , aes(x = time, y = x), color = "blue") 
                gy <- gy + geom_path(data = out , aes(x = time, y = y), color = "blue") 
                gu <- gu + geom_path(data = out , aes(x = time, y = u), color = "blue") 
                gv <- gv + geom_path(data = out , aes(x = time, y = v), color = "blue") 
        }
        gx <- gx + geom_path(data = Rout , aes(x = time, y = x), color = "black",size=1.2)
        gy <- gy + geom_path(data = Rout , aes(x = time, y = y), color = "black",size=1.2)
        gu <- gu + geom_path(data = Rout , aes(x = time, y = u), color = "black",size=1.2)
        gv <- gv + geom_path(data = Rout , aes(x = time, y = v), color = "black",size=1.2)     
        gx <- gx + labs(x = "t", y = expression(paste(x[epsilon],'(t)')))
        gy <- gy + labs(x = "t", y = expression(paste(y[epsilon],'(t)')))
        gu <- gu + labs(x = "t", y = expression(paste(u[epsilon],'(t)')))
        gv <- gv + labs(x = "t", y = expression(paste(v[epsilon],'(t)')))
        gx <- gx + theme_bw(base_size = 12, base_family = "Helvetica")
        gy <- gy + theme_bw(base_size = 12, base_family = "Helvetica")
        gu <- gu + theme_bw(base_size = 12, base_family = "Helvetica")
        gv <- gv + theme_bw(base_size = 12, base_family = "Helvetica")
        theme <-  theme(panel.grid.major = element_line(size = 0.5, 
                                                       color = "grey"), 
                       axis.line = element_line(size = 0.7, 
                                                color = "black"), 
                       legend.position = c(0.85,0.7), 
                       text = element_text(size = 16,family="CM Roman"))
        gx <- gx + theme
        gy <- gy + theme
        gu <- gu + theme
        gv <- gv + theme
        ggsave("ggplot_cm.pdf", gx, width=6, height=5)
        embed_fonts("ggplot_cm.pdf", outfile="/home/josue/ICE/ThesisICE/images/chapter5/convergenciax4.pdf")
        ggsave("ggplot_cm.pdf", gy, width=6, height=5)
        embed_fonts("ggplot_cm.pdf", outfile="/home/josue/ICE/ThesisICE/images/chapter5/convergenciay4.pdf")
        ggsave("ggplot_cm.pdf", gu, width=6, height=5)
        embed_fonts("ggplot_cm.pdf", outfile="/home/josue/ICE/ThesisICE/images/chapter5/convergenciau4.pdf")
        ggsave("ggplot_cm.pdf", gv, width=6, height=5)
        embed_fonts("ggplot_cm.pdf", outfile="/home/josue/ICE/ThesisICE/images/chapter5/convergenciav4.pdf")
}

ic <- c(x = 0.8, y = 0.7, u = 0, v = 0)
eps <- seq(0.01,0.6,by = 0.06)
PlotforEps(ic,30,eps)
