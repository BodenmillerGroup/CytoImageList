#------------------------------------------------------------------------------
# Generic functions for the CytoImageList and Image classes
#------------------------------------------------------------------------------

#' @export
setGeneric("channelNames", 
    function(x) standardGeneric("channelNames"))

#' @export
setGeneric("channelNames<-",
    function(x, value) standardGeneric("channelNames<-"))

#' @export
setGeneric("channelData", 
           function(x) standardGeneric("channelData"))

#' @export
setGeneric("channelData<-",
           function(x, value) standardGeneric("channelData<-"))

#' @export
setGeneric("getImages",
    function(x, i) standardGeneric("getImages"))

#' @export
setGeneric("setImages<-",
    function(x, i, value) standardGeneric("setImages<-"))

#' @export
setGeneric("getChannels",
    function(x, i) standardGeneric("getChannels"))

#' @export
setGeneric("setChannels<-",
    function(x, i, value) standardGeneric("setChannels<-"))

#' @export
setGeneric("int_metadata",
           function(x) standardGeneric("int_metadata"))

#' @export
setGeneric("int_metadata<-",
           function(x, value) standardGeneric("int_metadata<-"))




