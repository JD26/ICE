#library(deSolve)
#library(ggplot2)

a <- 1; 
c <- 1; 
f <- 1;
e <-3;
g <- 1;
k <- 1;
h <- 1;
i <- 1;
h0 <- 1;
n <- 1;
m <- 1;

DP <- function (t, y, parms) {
        with(as.list(y), {
                dx <- a*x + c*x*y - f*x*z
                dy <- e*y - g*x*y - k*y*y
                dz <- z*( (h*x*max(0,1-h0*C)) - i ) 
                dC <- n - C*( m + (h*x*max(0,1-h0*C)) )
                list(c(dx, dy, dz, dC))
        })
}

yini <- c(x = 1, y = 1, z = 1, C = 0)

times <- seq(from = 0, to = 400, by = 0.01)
out <- ode(y = yini, times = times, func = DP,parms = NULL)


d = data.frame(out)
g <- ggplot()+
        geom_path(data = d, aes(time,x),linetype = 2)+
        geom_path(data = d, aes(time,y),linetype = 3)+
        geom_path(data = d, aes(time,z))
g


g <- ggplot()+
        geom_path(data = d, aes(time,y),linetype = 2)
g