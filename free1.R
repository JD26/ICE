#library(deSolve)
#library(ggplot2)

a <- 0; 
c <- 1; 
f <- 1;
e <- 2;
g <- 0.5;
k <- 1;
h <- 1;
i <- 1;

DP <- function (t, y, parms) {
        with(as.list(y), {
                dx <- a*x + c*x*y - f*x*z
                dy <- e*y - g*x*y - k*y*y
                dz <- h*x*z - z*i
                list(c(dx, dy, dz))
        })
}

yini <- c(x = 1, y = 1, z = 1)

times <- seq(from = 0, to = 100, by = 0.01)
out <- ode(y = yini, times = times, func = DP,parms = NULL)


d = data.frame(out)
g <- ggplot()+
        geom_path(data = d, aes(time,x),linetype = 2)+
        geom_path(data = d, aes(time,y),linetype = 3)+
        geom_path(data = d, aes(time,z))
g


#g <- ggplot()+
#        geom_path(data = d, aes(x,y),linetype = 2)
#g