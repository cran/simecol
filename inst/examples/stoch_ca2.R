########################################################################
## Stochastic Cellular Automaton
########################################################################
require(simecol)
CA <- new("gridModel",
    main = function(time, init, parms) {
      z <- init
      n <- nrow(z)
      m <- ncol(z)
      ret <- with(parms,{
        ## rule 1: reproduction
        ## 1.1 which cells are adult? (only adults can generate)
        ad <- ifelse(z >= adult & z < old, z, 0)
        dim(ad) <- c(n, m)
        ## 1.2 how much (weighted) adult neighbours has each cell?
        nb <- neighbours(ad, wdist = wdist)
        ## 1.3 a proportion of the seeds develops juveniles
        ## simplified version, you can also use probabilities
        genprob <- nb * runif(nb) * ci
        zgen  <- ifelse(z==0 & genprob >= 1, 1, 0)

        ## rule 2: growth and survival of juveniles
        zsurvj <- ifelse(z >= 1     & z < adult & runif(z) <= pj, z+1, 0)
        ## rule 2: growth and survival of adults
        zsurva <- ifelse(z >= adult & z < old   & runif(z) <= pa, z+1, 0)
        ## rule 2: growth and survival of senescent
        zsurvs <- ifelse(z >= old               & runif(z) <= ps, z+1, 0)

        ## make resulting grid of complete population
        z     <- zgen + zsurvj + zsurva + zsurvs
        if (max(z)==0) stop("extinction", call.=FALSE)
        dim(z)  <-c(n,m)
        return(z)
      })
    },
    parms = list(
      wdist = matrix(c(0.5,0.5,0.5,0.5,0.5,
                  0.5,1.0,1.0,1.0,0.5,
                  0.5,1.0,1.0,1.0,0.5,
                  0.5,1.0,1.0,1.0,0.5,
                  0.5,0.5,0.5,0.5,0.5),nrow=5),
      pj = 0.99,  # survival probability of juveniles
      pa = 0.99,  # survival probability of adults
      ps = 0.1,   # survival probability of senescent
      ci = 1.0,   # "seeding constant"
      adult = 5,  # age of adolescence
      old   = 10  # age of senescence
    ),
    times = c(from=1, to=100, by=1),
    init = matrix(0, nrow=80, ncol=80),
    solver = "iteration",
    initfunc = function(obj) {
      init(obj)[38:42,38:42] <- 5 # deterministic seed in the middle of the grid
      obj
  }
)



mycolors <- function(n) {
  col <- c("wheat","darkgreen")
  if (n>2) col <- c(col, heat.colors(n-2))
  col
}

