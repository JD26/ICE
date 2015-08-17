rm(list = ls())

TestA <- matrix(c(0,-1,1,1,
                  0,-1,-1,0,
                  0,0,0,1,
                  0,-1,0,-1),nrow=4, byrow =T)

ErrorInput <- function(A){
	if (class(A) != "matrix") {
		stop("Input a community matrix.")
	}

	if ( ( nrow(A) != ncol(A) ) | nrow(A) == 0  ){
		stop("Input a no empty square community matrix.")	
	}	
}

ParticularCase <- function(A) {
	if ( nrow(A) == 1 ) {
		if ( A[1] <= 0) {
			return(TRUE)
		}
		return(FALSE)
	}
}

SignSemiStable <- function(A){
	return(fSS)
}

fSS <- function(A = TestA, imp = T){
	
	#
	ErrorInput(A)
	ParticularCase(A)
	#

	nSp <- nrow(A)	
	SelfLoop <- rep(0, nSp)	
	for ( i in 1:nSp ) {
			if ( A[i,i] > 0) {
				#Fail Alfa test
				return(FALSE)
			}
	        SelfLoop[i] <- sign(A[i,i])	        
	}
	if (imp){
		print("SelfLoop")
		print(SelfLoop)	
	}
	
	nLinks <- 0
	for ( i in 1:(nSp-1) ) {
	        for ( j in (i+1):nSp ) {
	        		if ( A[i,j]*A[j,i] > 0 ){
	        			#Fail Beta test
	        			return(FALSE)
	        		}
	                if ( A[i,j] != 0 ){
	                        nLinks <- nLinks + 1
	                }
	                if ( A[j,i] != 0 ){
	                        nLinks <- nLinks + 1
	                }
	        }
	}
	if (imp){
		print("nLinks")
		print(nLinks)
	}

	if ( nLinks == 0 ) {
		return(TRUE)
	}
	
	iTo <- rep(0, nSp + 1)
	iTo[1] <- 1
	iFrom <- rep(0, nSp + 1)
	iFrom[1] <- 1
	ToSp <- rep(0, nLinks)
	FromSp <- rep(0, nLinks)

	for (j in 1:nSp) {
	        count <- 0
	        to <- NULL	        
	    	    for ( i in 1:nSp ) {
	                if ( i != j & A[i, j] != 0 ) {
	                        to <- c(to, i)	                        
	                        count <- count + 1
	                }
	        }
	        if ( count > 0 ) {	        		
	                iTo[j+1] <- iTo[j] + count
	                ToSp[iTo[j]:(iTo[j+1]-1)] <- to	                
	        } else {
	                iTo[j+1] <- iTo[j]
	        }
	}

	for (i in 1:nSp) {
	        count <- 0	        
	        from <- NULL
	        for ( j in 1:nSp ) {
	                if ( i != j & A[i, j] != 0 ) {	                        
	                        from <- c(from, j)
	                        count <- count + 1
	                }
	        }
	        if ( count > 0 ) {	        		
	                iFrom[i+1] <- iFrom[i] + count	                
	                FromSp[iFrom[i]:(iFrom[i+1]-1)] <- from
	        } else {
	                iFrom[i+1] <- iFrom[i]
	        }
	}

	if (imp){
		print("ToSp")
		print(ToSp)
		print("FromSp")
		print(FromSp)
	}


	Visited <- rep(FALSE, nSp)
	Father <- rep(0,nSp)

	AffectedFrom <- function(Sp) {
		if ( iTo[Sp] < iTo[Sp+1] ) {
	        return(ToSp[iTo[Sp]:(iTo[Sp+1]-1)])
	    }
	    return(NULL)	    
	}

	AffectTo <- function(Sp) {
	    if ( iFrom[Sp] < iFrom[Sp+1] ) {
	        return(FromSp[iFrom[Sp]:(iFrom[Sp+1]-1)])
	    }
	    return(NULL)
	}

	ExistsPath <- function(Sp){
		From <- AffectedFrom(Sp)
		To <- AffectTo(Sp)
		
			for ( f in From ) {
				for ( t in To ) {
					if ( f != t ) {
						return(TRUE)
					}
				}
			}
		
		return(FALSE)
	}

	ExistsOthers <- function(Sp, FatherSp){		
		Affected <- AffectedFrom(Sp)		
		for ( SpAf in Affected ) {
				if ( SpAf != FatherSp ) {
					return(TRUE)
				}
		}
		return(FALSE)
	}

	SearchLoop <- function(Sp, throughSp) {
	    Visited[Sp] <<- TRUE	    
	    AffectedBySp <- ToSp[iTo[Sp]:(iTo[Sp+1]-1)]	    
	    for (SpAffected in AffectedBySp) {     		 
	        if ( SpAffected != Father[Sp] ) {	        	      	        	
	            if ( !(SpAffected %in%  throughSp) ) {
	                Father[SpAffected] <<- Sp
	                if ( ExistsOthers(SpAffected, Sp) ){	                              	
	                	if(SearchLoop(SpAffected, c(throughSp, SpAffected))){	                		
	                		return(TRUE)
	                	}
	                }                
	            } else {	            	          
	                return(TRUE)
	            }
	        }
	    }
	    return(FALSE)
	}
	

	
	for (Sp in 1:nSp) {
	            if ( !Visited[Sp] & ExistsPath(Sp) ) {							
	                       if ( SearchLoop(Sp, throughSp <- c() ) ) {
	                            return(FALSE)
	                       }                   
	            }
	}

	return(TRUE)
}



