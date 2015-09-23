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

alfa <- 4/365 # (1/dia)
xi1 <- 0.022 # (g/ug)
mu <- 1/365 # (1/dia)
beta <- 0.047 # (1/dia)
gamma <- 0.1 # (L/g)
xi2 <- 33.41 # (g/μg)
delta <- 0.00057 # (1/dia)
a1 <- 0.55 # (L/g/dia)
TX <- 0.25# (ug/L)
sigma1 <- 0.12 # (1/dia)
a2 <- 0.1733 # (L/g/dia)
sigma2 <- 0.0062 # (1/dia)

# funciones

F1 <- function(x,y,u,v) {
        alfa*max(0,1 - xi1*u)*x - mu*x - beta*x*y
}

F2 <- function(x,y,u,v) {
        gamma*max(0,1 - xi2*v)*beta*x*y - delta*y
        
}
G1 <- function(x,y,u,v) {
        a1*TX - sigma1*u - alfa*max(0,1 - xi1*u)*u
}

G2 <- function(x,y,u,v) {
        a2*TX - sigma2*v - gamma*max(0,1 - xi2*v)*beta*x*v + beta*x*u
}

Model <- function (t, y, parms) {
        with(as.list(y), {
                dx <- F1(x,y,u,v)
                dy <- F2(x,y,u,v)
                du <- G1(x,y,u,v)
                dv <- G2(x,y,u,v)
                list(c(dx, dy, du, dv))
        })
}

ic <- c(x = 0.00003, y = 0.0004, u = 0, v = 0)
times <- seq(from = 0, to = 500, by = 0.05)
out <- ode(y = ic, times = times, func = Model,parms = NULL)
out = data.frame(out)
head(out)
g <- ggplot()
g <- g + geom_path(data = out , aes(x = time, y = x), color = "red") 
g <- g + geom_path(data = out , aes(x = time, y = y), color = "blue") 
g




