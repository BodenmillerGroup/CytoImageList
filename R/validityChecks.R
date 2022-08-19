# Function to check if CytoImageList elements can be correctly replaced
#' @importFrom methods is
.valid.Image.setting <- function(x, i, value){
    # Check if value is Image or CytoImageList
    if(!is.null(value) && !(is(value, "Image_OR_DelayedArray") ||
        is(value, "CytoImageList"))){
        stop("Invalid replacement operation: \n",
            "Only 'Image' or 'CytoImageList' objects allowed.")
    }

    # If i is not character, both x and value need to be named,
    # or both x and value need to be unnamed
    error <- c()
    if(!is.null(value)){
        if(!is.character(i)){
            if(is.null(names(x))){
                if(is(value, "CytoImageList") && !is.null(names(value))){
                    error <- paste("Cannot merge named and",
                                    "unnamed CytoImageList object.")
                }
            } else {
                if(is(value, "Image_OR_DelayedArray")){
                    error <- "Cannot set Image object to named CytoImageList."
                } else if(is.null(names(value))){
                    error <- paste("Cannot merge named and",
                                    "unnamed CytoImageList object.")
                }
            }
        } else if (is.character(i)) {
            if(is.null(names(x))){
                error <- paste("'i' is of type character. \n",
                "This setting is only allowed for named CytoImageList objects.")
            }
        }
    }

    if(length(error > 0L)){
        stop("Invalid replacement operation: \n",
            error)
    }
}

# Check if channels can be replaced
#' @importFrom methods is
.valid.Channel.setting <- function(x, i, value){
    # Only CytoImageList objects are supported
    if(!is.null(value) && !is(value, "CytoImageList")){
        stop("Invalid replacement operation: \n",
            "Only 'CytoImageList' objects allowed.\n",
            "To alter Image objects, see ?Image.")
    }

    # Check if replacement has the same length
    if(!is.null(value) && length(x) != length(value)){
        stop("Invalid replacement operation: \n",
            "Replacement needs to have same length as 'x'.")
    }

    # Check if names of x and value match
    if(!is.null(value) && !is.null(names(x)) && !is.null(names(value))){
        if(!identical(names(x), names(value))){
            stop("Invalid replacement operation: \n",
            "Names of 'x' and 'value' do not match.")
        }
    }

    # Check if number of channels is same as length(i)
    if(!is.null(value) && length(i) != dim(value[[1]])[3]){
        stop("Invalid replacement operation: \n",
            "Number of replacement channels is not the same as \n",
            "number of channels to replace.")
    }

    # Check if channelNames are set if is.character(i)
    if(is.character(i) && is.null(channelNames(x))){
        stop("Invalid replacement operation: \n",
            "Trying to set a named channel in an unnamed CytoImageList.")
    }
}