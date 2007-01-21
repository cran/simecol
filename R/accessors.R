## accessor and replacement functions for simObj slots

setGeneric("parms", function(obj, ...) standardGeneric("parms"))
setGeneric("parms<-", function(obj, value) standardGeneric("parms<-"))

setGeneric("init", function(obj, ...) standardGeneric("init"))
setGeneric("init<-", function(obj, value) standardGeneric("init<-"))

setGeneric("times", function(obj, ...) standardGeneric("times"))
setGeneric("times<-", function(obj, value) standardGeneric("times<-"))

setGeneric("inputs", function(obj, ...) standardGeneric("inputs"))
setGeneric("inputs<-", function(obj, value) standardGeneric("inputs<-"))

setGeneric("equations", function(obj, ...) standardGeneric("equations"))
setGeneric("equations<-", function(obj, value) standardGeneric("equations<-"))

setGeneric("solver", function(obj, ...) standardGeneric("solver"))
setGeneric("solver<-", function(obj, value) standardGeneric("solver<-"))

setGeneric("main", function(obj, ...) standardGeneric("main"))
setGeneric("main<-", function(obj, value) standardGeneric("main<-"))

setGeneric("initfunc", function(obj, ...) standardGeneric("initfunc"))
setGeneric("initfunc<-", function(obj, value) standardGeneric("initfunc<-"))

setGeneric("out", function(obj, ...) standardGeneric("out"))
## the out slot is readonly

setMethod("parms", "simObj",
  function(obj) {
    obj@parms
  }
)

setMethod("parms<-", "simObj", 
    function(obj, value) {
      p <- obj@parms
      for (i in 1:length(value)) {
        ## ifelse(is.list ...) reserved for future extensions
        p[names(value[i])] <- ifelse(is.list(p), value[i], value[[i]])
      }
      na <- is.na(p)
      if (any(na)) p <- p[-which(na)]
      obj@parms <- p
      invisible(obj)
  }
)

setMethod("times", "simObj",
    function(obj) {obj@times}
)

setMethod("times<-", "simObj",
    function(obj, value) {
      if (isfromtoby(obj@times) & hasfromtoby(value)) {
        # modify named vector with from, to and by
        for (i in 1:length(value)) {
          nam <- names(value[i])
          if (nam %in% c("from", "to", "by")) {
            obj@times[nam] <- value[[i]]
          } else {
            print(paste("WARNING: vector element ", nam , " ignored"))
          }
        }
      } else {
        # overwrite vector
        if (is.null(names(value)) | isfromtoby(value)) {
          obj@times <- value
        } else {
          print("WARNING: Ignored! Invalid (or incomplete) names in right hand side of assignment.")
        }
      }
      invisible(obj)
    }
)

setMethod("init", "simObj",
    function(obj) {
      obj@init
    }
)

setMethod("init<-", "simObj",
    function(obj, value) {
      if (is.matrix(value) | is.data.frame(value)) {
        init <- value
      } else {
        init <- obj@init
        if (sum(is.na(match(names(value), names(init)))) > 0) {
          print("WARNING: additional names in right hand side of assignment")
        }
        for (i in 1:length(value)) {
          init[names(value[i])] <- value[[i]]
        }
        # delete elements which are NA
        na <- is.na(init)
        if (any(na)) init <- init[-which(na)]
      }
      obj@init <- init
      invisible(obj)
    }
)

setMethod("init<-", c("gridModel", "matrix"),
    function(obj, value) {
      obj@init <- value
      invisible(obj)
    }
)

setMethod("init<-", c("gridModel", "ANY"),
    function(obj, value) {
      print("error: value must be a matrix")
      invisible(obj)
    }
)

setMethod("inputs", "simObj",
    function(obj) {obj@inputs}
)

setMethod("inputs<-", "simObj",
    function(obj, value) {
      obj@inputs <- value
      invisible(obj)
    }
)

setMethod("main", "simObj",
    function(obj) {obj@main}
)

setMethod("main<-", "simObj",
    function(obj, value) {
      obj@main <- value
      invisible(obj)
    }
)

setMethod("equations", "simObj",
    function(obj) {obj@equations}
)

setMethod("equations<-", "simObj",
    function(obj, value) {
      for (i in 1:length(value)) {
        ## ifelse(is.list ...) reserved for future extensions
        obj@equations[names(value[i])] <- ifelse(is.list(obj@equations), value[i], value[[i]])
      }
      invisible(obj)
    }
)

setMethod("initfunc", "simObj",
    function(obj) {obj@initfunc}
)

setMethod("initfunc<-", "simObj",
    function(obj, value) {
      obj@initfunc <- value
      invisible(obj)
    }
)

setMethod("solver", "simObj",
    function(obj) {obj@solver}
)

setMethod("solver<-", "simObj",
    function(obj, value) {
      obj@solver <- value
      invisible(obj)
    }
)

setMethod("out", "simObj",
  # returns a list
  function(obj, last=FALSE) {
    o <- obj@out
    if (last) o[length(o)] else o
  }
)

setMethod("out", "gridModel",
  # if last==TRUE: returns a matrix (identical to init)
  function(obj, last=FALSE) {
    o <- obj@out
    if (last) o[[length(o)]] else o
  }
)

setMethod("out", "odeModel",
  # if last==TRUE: returns a vector (similar to init, time as first value)
  function(obj, last=FALSE) {
    o <- obj@out
    if (last) unlist(o[nrow(o), ]) else o
  }
)


