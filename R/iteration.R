iteration <- function(obj, animate = FALSE, ...) {
  if (is(obj, "simObj")) {
      init              <- obj@init
      times             <- fromtoby(obj@times)
      func              <- obj@main
      equations         <- obj@equations
      parms             <- obj@parms
      inputs            <- obj@inputs
      environment(func) <- environment()
    } else {
      print("Wrong class for iteration ...")
    }
    #if (!is.numeric(init))  stop("`init' must be numeric")
    if (!is.numeric(times)) stop("`times' must be numeric")
    if (!is.function(func)) stop("`func' must be a function")
    #if (!is.numeric(parms)) stop("`parms' must be numeric")

    out <- list(init)
  
    for (i in 1:length(times)) {
      time <- times[i]
      init    <- func(time, init, parms)
      out  <- c(out, list(init))
      if (animate) {
        obj@out   <- out
        plot(obj, index=i, ...)
      }
    }
    out
  }
