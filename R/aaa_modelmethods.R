
############################################################
#  The S4 stuff                                            #
############################################################

setClass("simObj",
         representation(
           main = "function",
           equations = "list",            
           times     = "numeric",
           init      = "ANY", #"numeric",
           parms     = "ANY",
           inputs    = "ANY",
           solver    = "character",                        
           out       = "ANY"
         )
)

setClass("odeModel",  
         representation(
           parms  = "numeric", 
           init   = "numeric"
         ), 
         contains = "simObj"
)

setClass("gridModel", 
         representation(
           parms  = "list", 
           init   = "matrix"
         ),  
         contains = "simObj"
)


setClass("rwalkModel", 
         representation(
           parms  = "list", 
           init   = "ANY" # or data frame or matrix???
         ),  
         contains = "simObj"
)


                                             


## Function for which generics already exist
          
setMethod("print", "simObj",
    function(x, ...) {
      base:::print(x, ...)
    }
)


