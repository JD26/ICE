library(gridGraphviz)
library(Rgraphviz)
library(graph)
#browseVignettes("Rgraphviz")
#graphvizCapabilities()$layoutTypes
#https://www.stat.auckland.ac.nz/~paul/Reports/gridGraphviz/gridGraphviz.html#ref-5
MatrixTest <- matrix(c(0, -4, 0, -2,
                       1, -2, 1,0 ,
                       0, -1, 0,0,
                       0, 0, 5, -3),
                     nrow = 4,
                     byrow = T)
rownames(MatrixTest) <- letters[1:4]
colnames(MatrixTest) <- letters[1:4]

MatrixPaper <- matrix(c(-1,0,1,
                        1,-1,0,
                        0,-1,-1), 
                      nrow=3,
                      byrow = T)
rownames(MatrixPaper) <- c(expression("IS"[2]),"IC","ID")
colnames(MatrixPaper) <- c("IS","IC","ID")

Head <- function(x){
        if (x > 0){
                return("normal")
        }
        if (x < 0){
                return("odot")
        }
        return("none")                
}
Sign <- function(x){
        if (x>0){
                return("+")
        }
        if (x<0){
                return("-")
        }
        return("")
}
isZero <- function(x){
        if(x!=0){
                return(1)
        }
        return(0)
}
G <- function(M){        
        n <- nrow(M)
        for (i in 1:(n-1)){
                for (j in (i+1):n){
                        if (isZero( M[i,j]) != isZero(M[j,i]) ){
                                M[i,j] <- 0
                                M[j,i] <- 0
                        }
                }
        }
        return(M)
}

GraphWeigth <- function(M = MatrixTest){
        n <- nrow(M)
        nodes <- colnames(M)
        link <- NULL
        label <- NULL
        for (i in 1:n){
                for (j in 1:n){
                        if (M[i,j] != 0){
                                link <- c(link, paste0(nodes[j],"~",nodes[i]))
                                label <- c(label, M[i,j])                                
                        }                        
                }
        }
        names(label) <- link                
        directedGraph <- graphAM(adjMat=t(abs(M)), edgemode = "directed")
        Ragraph <- agopen(directedGraph, "myGraph", 
                          recipEdges= "distinct",
                          attrs = list(graph=list(rankdir="LR",
                                                  fontsize="30",
                                                  nodesep="0.2"),
                                       edge = list(style="bold"),
                                       node = list(height="0.8", 
                                                   width="0.8", 
                                                   fontsize="30")),
                          edgeAttrs=list(label=label),
                          layoutType = "dot")               
        return(Ragraph)
}

GraphLevins <- function(M = MatrixTest){        
        n <- nrow(M)        
        arrowhead <- NULL
        for (i in 1:n){
                for (j in 1:n){
                        if (M[i,j] != 0){
                                arrowhead <- c(arrowhead, Head(M[i,j]))                                                                     
                        }                        
                }
        }                
        directedGraph <- graphAM(adjMat=t(abs(M)), edgemode = "directed")
        Ragraph <- agopen(directedGraph, "myGraph", 
                          recipEdges= "distinct",
                          attrs = list(graph=list(rankdir="LR",
                                                  fontsize="30",
                                                  nodesep="0.2"),
                                       edge = list(style="dotted"),
                                       node = list(height="0.8", width="0.8", fontsize="30")),
                          layoutType = "dot")        
        for (i in 1:length(arrowhead)){
                AgEdge(Ragraph)[[i]]@arrowhead <- arrowhead[i]       
        }        
        return(Ragraph)
}

GraphSign <- function(M = MatrixTest){
        n <- nrow(M)
        nodes <- colnames(M)
        link <- NULL
        label <- NULL
        for (i in 1:n){
                for (j in 1:n){
                        if (M[i,j] != 0){
                                link <- c(link, paste0(nodes[j],"~",nodes[i]))
                                label <- c(label, Sign(M[i,j]))                                
                        }                        
                }
        }
        names(label) <- link                
        directedGraph <- graphAM(adjMat=t(abs(M)), edgemode = "directed")
        Ragraph <- agopen(directedGraph, "myGraph", 
                          recipEdges= "distinct",
                          attrs = list(graph=list(rankdir="LR",
                                                  fontsize="30",
                                                  nodesep="0.2"),
                                       edge = list(style="bold"),
                                       node = list(height="0.8", 
                                                   width="0.8", 
                                                   fontsize="30")),
                          edgeAttrs=list(label=label),
                          layoutType = "dot")               
        return(Ragraph)
        
}

GraphEfect <- function(M = MatrixTest){
        label <- rep(" ",nrow(M))
        label[which(diag(M)<0)] <- "-"
        label[which(diag(M)>0)] <- "+"
        names(label) = rownames(M)
        diag(M) <- 0
        directedGraph <- graphAM(adjMat=t(abs(M)), edgemode = "directed")
        Ragraph <- agopen(directedGraph, "myGraph", 
                          recipEdges= "combined",
                          attrs = list(graph=list(rankdir="LR",
                                                  fontsize="30",
                                                  nodesep="0.2"),
                                       edge = list(style="bold",
                                                   arrowsize=2.3),
                                       node = list(height="0.8", 
                                                   width="0.8", 
                                                   fontsize="30")),
                          nodeAttrs=list(label=label),
                          layoutType = "dot")  
        for (i in seq(along = AgEdge(Ragraph))) {
                if (AgEdge(Ragraph)[[i]]@dir == "both"){
                        AgEdge(Ragraph)[[i]]@arrowtail <- "none"
                        AgEdge(Ragraph)[[i]]@arrowhead <- "none"
                }else{
                        AgEdge(Ragraph)[[i]]@arrowhead <- "normal"
                }                
        }
        return(Ragraph)
}

GraphG <- function(M = MatrixTest){
        label <- rep(" ",nrow(M))
        label[which(diag(M)<0)] <- "-"
        label[which(diag(M)>0)] <- "+"
        names(label) = rownames(M)
        diag(M) <- 0
        M <- G(M)
        directedGraph <- graphAM(adjMat=t(abs(M)), edgemode = "directed")
        Ragraph <- agopen(directedGraph, "myGraph", 
                          recipEdges= "combined",
                          attrs = list(graph=list(rankdir="LR",
                                                  fontsize="30",
                                                  nodesep="0.2"),
                                       edge = list(style="bold",
                                                   arrowsize=2.3),
                                       node = list(height="0.8", 
                                                   width="0.8", 
                                                   fontsize="30")),
                          nodeAttrs=list(label=label),
                          layoutType = "dot")  
        for (i in seq(along = AgEdge(Ragraph))) {
                AgEdge(Ragraph)[[i]]@arrowtail <- "none"
                AgEdge(Ragraph)[[i]]@arrowhead <- "none"              
        }
        return(Ragraph)
}

grid.graph(GraphWeigth(MatrixPaper), newpage=TRUE)
grid.graph(GraphLevins(), newpage=TRUE)
grid.graph(GraphSign(), newpage=TRUE)
grid.graph(GraphEfect(), newpage=TRUE)
grid.graph(GraphG(), newpage=TRUE)




