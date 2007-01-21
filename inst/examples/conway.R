########################################################################
## Deterministic Cellular Automaton
##   Conway's Game of Life
########################################################################
conway <- new("gridModel",
  main = function(time, init, parms) {
    x   <- init
    srv <- parms$srv
    gen <- parms$gen
    n   <- nrow(x)
    m   <- ncol(x)
    nb  <- eightneighbours(x)
    ## survival rule
    xsrv <- ifelse(x > 0 & (nb %in% srv), 1, 0)
    ## generation rule
    xgen <- ifelse(x == 0 & (nb %in% gen), 1, 0)
    x    <- as.numeric((xgen + xsrv)>0)
    dim(x) <- c(n,m)
    x
  },
  parms = list(srv=c(2, 3), gen=3),
  times = c(from=1, to=10, by=1),
  init = matrix(round(runif(40*40)), nrow=40, ncol=40),
  solver = "iteration"
)



