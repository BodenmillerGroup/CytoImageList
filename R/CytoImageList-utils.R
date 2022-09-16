# Utility functions for CytoImageList and Image class objects -----------------

#' @title Getting and setting the channel and image names
#' @name CytoImageList-naming
#'
#' @description
#' Methods to get and set the names of individual channels or the names of
#' individual images.
#'
#' @section Setting and getting the channel names:
#' In the following code, \code{x} is a \linkS4class{CytoImageList}
#' object containing one or multiple channels. The channel
#' names can be replaced by \code{value}, which contains a character vector of
#' the same length as the number of channels in the images.
#' \describe{
#' \item{\code{channelNames(x)}}{Returns the names of all channels stored in
#' \code{x}}
#' \item{\code{channelNames(x) <- value}}{Replaces the channel names of
#' \code{x} with \code{value}. For this, \code{value} needs to have the same
#' length as the number of channels in \code{x}}
#' }
#' 
#' @section Setting and getting the channel data:
#' \code{x} is a \linkS4class{CytoImageList}
#' object containing images with one or multiple channels. The 
#' \code{channelData} getter/setter function accesses channel-specific
#' metadata.
#' \describe{
#' \item{\code{channelData(x)}}{Returns a \code{DataFrame} storing the
#' channel-specific metadata stored in \code{x}}
#' \item{\code{channelData(x) <- value}}{Replaces the channel metdata of
#' \code{x} with \code{value}. For this, \code{value} needs to have the same
#' number of rows as the number of channels in \code{x} and need to be 
#' coercible to a \code{DataFrame}.}
#' }
#'
#' @section Setting and getting the image names:
#' Here, \code{x} is a \linkS4class{CytoImageList} object. The element
#' names can be replaced by \code{value}, which contains a character vector of
#' the same length as the number of images. In case of the CytoImageList object,
#' elements are always images.
#' \describe{
#' \item{\code{names(x)}}{Returns the names of all images stored in \code{x}}
#' \item{\code{names(x) <- value}}{Replaces the image names of
#' \code{x} with \code{value}. For this, \code{value} needs to have the same
#' length as \code{x}}
#' }
#'
#' @examples
#' library(cytomapper)
#' data("pancreasImages")
#'
#' # Get channel and image names
#' channelNames(pancreasImages)
#' names(pancreasImages)
#'
#' # Set channel and image names
#' channelNames(pancreasImages) <- paste0("marker", 1:5)
#' names(pancreasImages) <- paste0("image", 1:3)
#' 
#' # Set and get channel metadata
#' channelData(pancreasImages) <- DataFrame(channel = paste0("channel", 1:5))
#' channelData(pancreasImages)
#'
#' @aliases
#' channelNames channelNames<-
#' channelNames,CytoImageList-method
#' channelNames<-,CytoImageList-method
#' names,CytoImageList-method
#' names<-,CytoImageList-method
#'
#' @docType methods
#'
#' @author
#' Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
NULL

#' @export
setMethod("channelNames",
    signature = signature(x="CytoImageList"),
    definition =  function(x){
    if(length(dim(x[[1]])) == 2L){
        return(NULL)
    } else {
        return(dimnames(x[[1]])[[3]])
    }
})

#' @export
#' @importFrom S4Vectors endoapply
#' @importFrom EBImage Image
#' @importFrom methods validObject
setReplaceMethod("channelNames",
    signature = signature(x="CytoImageList"),
    definition = function(x, value){

    if (is(x[[1]], "Image")) {
        # Image needs to be expanded to store channel names
        if(length(dim(x[[1]])) == 2L){
            x <- S4Vectors::endoapply(x, function(y){
                cur_Image <- Image(y, dim = c(dim(y)[1], dim(y)[2], 1))
                dimnames(cur_Image) <- c(dimnames(y), NULL)
                return(cur_Image)
            })
        }

        x <- S4Vectors::endoapply(x, function(y){
            if (is.null(value)) {
                dimnames(y)[[3]] <- NULL
            } else {
                dimnames(y)[[3]] <- as.character(value)
            }
            return(y)
        })
    } else {
        # Image needs to be expanded to store channel names
        if(length(dim(x[[1]])) == 2L){
            x@listData <- lapply(x, function(y){
                cur_Image <- y
                dim(cur_Image) <- c(dim(cur_Image)[1], dim(cur_Image)[2], 1)
                dimnames(cur_Image) <- c(dimnames(y), NULL)
                return(cur_Image)
            })
        }

        x@listData <- lapply(x, function(y){
            if (is.null(value)) {
                dimnames(y)[[3]] <- NULL
            } else {
                dimnames(y)[[3]] <- as.character(value)
            }
            return(y)
        })
    }

    validObject(x)

    return(x)
    })

#' @export
#' @importFrom methods callNextMethod
setMethod("names",
    signature = signature(x="CytoImageList"),
    definition = function(x){
        callNextMethod()
    })

#' @export
#' @importFrom methods callNextMethod as validObject
setReplaceMethod("names",
    signature = signature(x="CytoImageList"),
    definition = function(x, value){
        .Object <- callNextMethod()
        .Object <- as(.Object, "CytoImageList")
        validObject(.Object)
        return(.Object)
    })

#' @title Getting and setting the channel metadata
#' @name channelData
#'
#' @description
#' Methods to get and set the metdata of individual channels.
#'
#' In the following code \code{x} is a \linkS4class{CytoImageList}
#' object containing images with one or multiple channels. The 
#' \code{channelData} getter/setter function accesses channel-specific
#' metadata.
#' \describe{
#' \item{\code{channelData(x)}}{Returns a \code{DataFrame} storing the
#' channel-specific metadata stored in \code{x}}
#' \item{\code{channelData(x) <- value}}{Replaces the channel metdata of
#' \code{x} with \code{value}. For this, \code{value} needs to have the same
#' number of rows as the number of channels in \code{x} and need to be 
#' coercible to a \code{DataFrame}.}
#' }
#'
#' @examples
#' library(cytomapper)
#' data("pancreasImages")
#'
#' channelData(pancreasImages) <- DataFrame(channel = paste0("channel", 1:5))
#' channelData(pancreasImages)
#'
#' @aliases
#' channelData channelData<-
#' channelData,CytoImageList-method
#' channelData<-,CytoImageList-method
#'
#' @docType methods
#'
#' @author
#' Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
NULL

#' @export
setMethod("channelData",
          signature = signature(x="CytoImageList"),
          definition =  function(x){
              x@channelData    
          })

#' @export
#' @importFrom S4Vectors endoapply
#' @importFrom EBImage Image
#' @importFrom methods validObject
setReplaceMethod("channelData",
                 signature = signature(x="CytoImageList"),
                 definition = function(x, value){
                     
                     if (!is(value, "DataFrame")) {
                         value <- as(value, "DataFrame")
                     }
                     
                     if (nrow(value) != dim(x[[1]])[3]) {
                         stop("'nrow(value)' need to match the number of channels.")
                     }    
                     
                     if (!is.null(channelNames(x))) {
                         rownames(value) <- channelNames(x)
                     }
                     
                     x@channelData <- value    
                     
                     validObject(x)
                     
                     return(x)
                 })

#' @title Update a CytoImageList object to the newest version
#'
#' @description Updates the version of a \code{CytoImageList} object by
#' adjusting missing slots.
#'
#' @param object an old \code{CytoImageList} object
#'
#' @details #TODO
#'
#' @return An updated CytoImageList object
#'
#' @examples
#' library(cytomapper)
#' data("pancreasImages")
#' 
#' pancreasImages <- updateVersion(pancreasImages)
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#'
#' @name updateObject
#' @export
#' @aliases updateObject updateObject,CytoImageList-method
#' @importFrom BiocGenerics updateObject
#' @importFrom utils packageVersion
setMethod("updateObject", "CytoImageList", function(object) {
    old.ver <- int_metadata(object)$version
    old.pkg <- attr(class(object), "package")
    
    if (old.pkg == "cytomapper") {
        object <- CytoImageList(object)
        int_metadata(object)$version <- packageVersion("cytomapper")
    }
    
    callNextMethod()
})

#' @title Internal setters and getters
#'
#' @description
#' Methods to get and set the internal metadata of a \code{CytoImageList} object.
#' The function is not meant for direct use.
#'
#' In the following code the \code{int_metadata} getter/setter function accesses and replaces 
#' internal metadata.
#' \describe{
#' \item{\code{int_metadata(x)}}{Returns a \code{list} storing the
#' internal metadata of \code{x}}
#' \item{\code{int_metadata(x) <- value}}{Replaces internal metadata of
#' \code{x} with \code{value}. For this, \code{value} needs to be a \code{list}.}
#' }
#'
#' @return A CytoImageList object
#'
#' @examples
#' library(cytomapper)
#' data("pancreasImages")
#' 
#' int_metadata(pancreasImages) 
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#'
#' @name int_metadata
#' 
#' @export
#' 
#' @aliases
#' int_metadata int_metadata<-
#' int_metadata,CytoImageList-method
#' int_metadata<-,CytoImageList-method
#' 
#' @docType methods
#'
#' @author
#' Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
NULL

#' @export
setMethod("int_metadata", "CytoImageList", function(x) x@int_metadata)

#' @export
setReplaceMethod("int_metadata", "CytoImageList", function(x, value) {
    x@int_metadata <- value
    
    validObject(x)
    
    return(x)
})
