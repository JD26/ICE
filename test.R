
validate.c <- function(C) {
        if (!(is.matrix(C)) | !(identical(nrow(C), ncol(C)))) {
                stop("\nThe system must be specified by a square matrix with elements \nof values of only 1, 0 and -1.")
        }
        for (i in 1:nrow(C)) {
                for (j in 1:ncol(C)) {
                        if (!((C[i, j] == 1) | (C[i, j] == 0) | (C[i, 
                                                                   j] == -1))) {
                                stop("\nThe system must be a square matrix with elements \nof values of only 1, 0 and -1.")
                        }
                }
        }
}
sosl.prod <- function(C, SOSL) {
        loop.prod <- function(C, loop) {
                lprod <- 1
                if (length(loop) > 1) {
                        for (edge in 1:(length(loop))) {
                                if (edge < length(loop)) {
                                        lprod <- lprod * C[loop[edge], loop[edge + 
                                                                                    1]]
                                }
                                else {
                                        lprod <- lprod * C[loop[edge], loop[1]]
                                }
                        }
                }
                if (length(loop) == 1) {
                        lprod <- lprod * C[loop[1], loop[1]]
                }
                return(lprod)
        }
        sprod <- 1
        for (loop in SOSL) {
                sprod <- sprod * loop.prod(C, loop)
        }
        return(sprod)
}
make.MOSL <- function(LOL, N) {
        MOSL <- rep(list(NA), N)
        for (j in 1:N) {
                MOSL[[j]] <- rep(list(NA), (N - (j - 1)))
        }
        for (a in 1:length(LOL)) {
                if (identical((MOSL[[LOL[[a]][1]]][[length(unique(LOL[[a]]))]]), 
                              NA)) {
                        MOSL[[LOL[[a]][1]]][[length(unique(LOL[[a]]))]] <- list(as.double(unique(LOL[[a]])))
                }
                else {
                        MOSL[[LOL[[a]][1]]][[length(unique(LOL[[a]]))]] <- c(MOSL[[LOL[[a]][1]]][[length(as.double(unique(LOL[[a]])))]], 
                                                                             list(unique(LOL[[a]])))
                }
        }
        return(MOSL)
}
enumerate.SOSL <- function(MOSL, N) {
        set.size <- function(PLOS) {
                size <- 0
                if (length(PLOS) == 0) {
                        return(size)
                }
                for (q in 1:length(PLOS)) {
                        size <- size + length(PLOS[[q]])
                }
                return(size)
        }
        make.loopENVY <- function(PLOS, MOSL) {
                if (length(PLOS) == 0) {
                        return(seq(1:length(MOSL)))
                }
                max.search.space <- seq(1:length(MOSL))
                search.row <- max.search.space
                for (x in 1:length(PLOS)) {
                        search.row <- setdiff(search.row, PLOS[[x]])
                }
                return(search.row)
        }
        initialize.term <- function(MOSL) {
                N1 <- length(MOSL)
                Term <- MOSL
                for (i in 1:length(MOSL)) {
                        for (j in 1:length(MOSL[[i]])) {
                                Term[[i]][[j]] <- c(rep(0, length(MOSL[[i]][[j]])))
                        }
                }
                return(Term)
        }
        N.mosl <- length(MOSL)
        Term <- initialize.term(MOSL)
        PLOS <- NULL
        SOSL <- NULL
        k.last <- NULL
        search.row <- function(PLOS) {
                if (length(PLOS) == 0) {
                        return(1)
                }
                row <- seq(1:N.mosl)
                for (x in 1:length(PLOS)) {
                        row <- setdiff(row, PLOS[[x]])
                }
                return(row[[1]])
        }
        search.over <- function(row) {
                if (row == 1) {
                        for (j in 1:length(MOSL[[1]])) {
                                for (k in 1:length(MOSL[[1]][[j]])) {
                                        if (Term[[1]][[j]][[k]] == 0) {
                                                return(FALSE)
                                        }
                                }
                        }
                        return(TRUE)
                }
                return(FALSE)
        }
        next.loop <- function(row) {
                for (j in 1:length(MOSL[[row]])) {
                        for (k in 1:length(MOSL[[row]][[j]])) {
                                if ((Term[[row]][[j]][[k]] == 0) & (!is.na(MOSL[[row]][[j]][[k]][[1]])) & 
                                            (length(MOSL[[row]][[j]][[k]]) <= N.mosl - 
                                                     set.size(PLOS))) {
                                        return(list(MOSL[[row]][[j]][[k]], k))
                                }
                        }
                }
                return(list(NULL, NULL))
        }
        while (TRUE) {
                row <- search.row(PLOS)
                if (search.over(row)) {
                        return(SOSL)
                }
                if (length(row) == 0) {
                        return(SOSL)
                }
                loop <- next.loop(row)
                if (is.null(loop[[1]]) & row == 1) {
                        return(SOSL)
                }
                if (is.null(loop[[1]]) & row != 1) {
                        for (i in make.loopENVY(PLOS, MOSL)) {
                                for (j in 1:length(MOSL[[i]])) {
                                        for (k in 1:length(MOSL[[i]][[j]])) {
                                                Term[[i]][[j]][[k]] <- 0
                                        }
                                }
                        }
                        i <- row
                        j <- length(MOSL[[row]])
                        for (k in 1:length(MOSL[[i]][[j]])) {
                                Term[[i]][[j]][[k]] <- 0
                        }
                        i <- PLOS[[length(PLOS)]][[1]]
                        j <- length(PLOS[[length(PLOS)]])
                        k <- loop[[2]]
                        for (jj in 1:j) {
                                for (kk in 1:length(Term[[i]][[jj]])) {
                                        if (jj == j & kk <= k.last[[length(k.last)]]) {
                                                Term[[i]][[jj]][[kk]] <- 1
                                        }
                                }
                        }
                        PLOS <- PLOS[1:length(PLOS) - 1]
                        k.last <- k.last[1:length(k.last) - 1]
                        next
                }
                if (is.null(PLOS)) {
                        PLOS <- list(loop[[1]])
                        k.last <- c(k.last, list(loop[[2]]))
                }
                else {
                        PLOS <- c(PLOS, list(loop[[1]]))
                        k.last <- c(k.last, list(loop[[2]]))
                }
                if (set.size(PLOS) == N) {
                        if (is.null(SOSL)) {
                                SOSL <- list(PLOS)
                        }
                        else {
                                SOSL <- c(SOSL, list(PLOS))
                        }
                        i <- PLOS[[length(PLOS)]][[1]]
                        j <- length(PLOS[[length(PLOS)]])
                        k <- loop[[2]]
                        Term[[i]][[j]][[k.last[[length(k.last)]]]] <- 1
                        PLOS <- PLOS[1:length(PLOS) - 1]
                        k.last <- k.last[1:length(k.last) - 1]
                        next
                }
        }
}
C = submatrix
validate.c(C)
N <- nrow(C)
if (is.null(nrow(C))) {
        print(C)
}
LOL <- (enumerate.loops(C))
if (is.null(LOL)) {
        return(0)
}
Sum <- 0
for (SOSL in enumerate.SOSL(make.MOSL(LOL, N), N)) {
        print("__________________________________________________")
        adjust <- (-1)^(length(SOSL) + 1)
        print(SOSL)
        sprod <- sosl.prod(C, SOSL) * adjust
        print("sprod:")
        print( sosl.prod(C, SOSL))
        if (Sum == 0) {
                Sum <- sprod
        }
        else {
                if (Sum == -1 * sprod) {
                        print("NA")
                }
        }
}


jojo <- function(v) {
        Sum <- 0
        for (SOSL in v) {
                
                sprod <- SOSL
                if (Sum == 0) {
                        Sum <- sprod
                }
                else {
                        if (Sum == -1 * sprod) {
                                return(NA)
                        }
                }
        }
        return(Sum)  
}






