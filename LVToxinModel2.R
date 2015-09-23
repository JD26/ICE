vector_field <- function(
        f,  # Function describing the vector field
        xmin=0, xmax=1, ymin=0, ymax=1,
        width=600, height=600,
        iterations=50,
        epsilon=.01,
        trace=TRUE
) {
        z <- matrix(runif(width*height),nr=height)
        i_to_x <- function(i) xmin + i / width  * (xmax - xmin)
        j_to_y <- function(j) ymin + j / height * (ymax - ymin)
        x_to_i <- function(x) pmin( width,  pmax( 1, floor( (x-xmin)/(xmax-xmin) * width  ) ) )
        y_to_j <- function(y) pmin( height, pmax( 1, floor( (y-ymin)/(ymax-ymin) * height ) ) )
        i <- col(z)
        j <- row(z)
        x <- i_to_x(i)
        y <- j_to_y(j)
        res <- z
        for(k in 1:iterations) {
                v <- matrix( f(x, y), nc=2 )
                x <- x+.01*v[,1]
                y <- y+.01*v[,2]
                i <- x_to_i(x)
                j <- y_to_j(y)
                res <- res + z[cbind(i,j)]
                if(trace) {
                        cat(k, "/", iterations, "\n", sep="")
                        dev.hold()
                        image(res)
                        dev.flush()
                }
        }
        if(trace) {
                dev.hold()
                image(res>quantile(res,.6), col=0:1)
                dev.flush()
        }
        res
}

# Sample data
van_der_Pol <- function(x,y, mu=1) c(y, mu * ( 1 - x^2 ) * y - x )
res <- vector_field(
        van_der_Pol,
        xmin=-3, xmax=3, ymin=-3, ymax=3,
        width=800, height=800,
        iterations=50,
        epsilon=.01
)
image(-res)

xcp <- delta/gamma
ycp <- 1-mu;

DE <- function (t, y, parms) {
        with(as.list(y), {
                dx <- max(0,1-TX)*x - mu*x - x*y
                dy <- gamma*max(0,1-(TX*c/sigma)-(TX*beta/sigma)*x)*x*y-delta*y
                list(c(dx, dy))
        })
}

X0 <- c(x = 0.2, y = 0.1)
times <- seq(from = 0, to = 100, by = 0.01)
data <- ode(y = X0, times = times, func = DE,parms = NULL)

data = data.frame(data)
colnames(data) = c("time","prey","predator")
data1 = data
data <- melt(data, id=c("time"))
colnames(data) = c("time","variable","biomass")



g <- ggplot()+
        geom_path(data = data ,
                  aes(x = time, y = biomass, colour = variable))  
g

g <- ggplot()+
        geom_path(data = data1 ,
                  aes(x = prey, y = predator),)  
g



conditions()

vector_field <- function(
        f,  # Function describing the vector field
        xmin=0, xmax=1, ymin=0, ymax=1,
        width=600, height=600,
        iterations=20,
        epsilon=.01,
        trace=TRUE
) {
        z <- matrix(runif(width*height),nr=height)
        i_to_x <- function(i) xmin + i / width  * (xmax - xmin)
        j_to_y <- function(j) ymin + j / height * (ymax - ymin)
        x_to_i <- function(x) pmin( width,  pmax( 1, floor( (x-xmin)/(xmax-xmin) * width  ) ) )
        y_to_j <- function(y) pmin( height, pmax( 1, floor( (y-ymin)/(ymax-ymin) * height ) ) )
        i <- col(z)
        j <- row(z)
        x <- i_to_x(i)
        y <- j_to_y(j)
        res <- z
        for(k in 1:iterations) {
                v <- matrix( f(x, y), nc=2 )
                x <- x+.01*v[,1]
                y <- y+.01*v[,2]
                i <- x_to_i(x)
                j <- y_to_j(y)
                res <- res + z[cbind(i,j)]
                if(trace) {
                        cat(k, "/", iterations, "\n", sep="")
                        dev.hold()
                        image(res)
                        dev.flush()
                }
        }
        if(trace) {
                dev.hold()
                image(res>quantile(res,.6), col=0:1)
                dev.flush()
        }
        res
}

mu <- 0.01
beta <- 2
gamma <- 0.3
c <- 0.3
sigma <- 1
delta <- 0.2
TX <- 0.16

van_der_Pol <- function(x,y) {
        c(max(0,1-TX)*x - mu*x - x*y,gamma*max(0,1-(TX*c/sigma)-(TX*beta/sigma)*x)*x*y-delta*y)       
}
res <- vector_field(
        van_der_Pol,
        xmin=0, xmax=3, ymin=0, ymax=1.5,
        width=800, height=800,
        iterations=20,
        epsilon=.01
)
image(-res)



require(maps)
map("world",xlim=c(110,155),ylim=c(-40,-10))
par(xpd=TRUE)
text(132,-5,"Approximate magnetic deviation - Australia",cex=1.5)
par(xpd=FALSE)
long<-rep(seq(117.5,152.5,by=5),6)
lat<-rep(c(-12.5,-17.5,-22.5,-27.5,-32.5,-37.5),each=8)
# just show the direction, don't have a magnitude difference
mag<-rep(1,48)
devdeg<-c(110,98,85,65,65,65,65,65,
          115,100,90,80,72,66,63,55,
          130,100,90,82,72,67,62,54,
          122,111,95,86,70,67,56,48,
          118,116,110,87,74,68,62,45,
          128,115,107,90,78,66,53,45)
vectorField(devdeg,mag,long,lat,scale=0.7,vecspec="deg")