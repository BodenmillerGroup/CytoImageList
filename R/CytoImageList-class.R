#' @export
#' @rdname CytoImageList
#' @importFrom utils packageVersion
#' @importFrom S4Vectors setValidity2
#' @importClassesFrom S4Vectors SimpleList DataFrame
setClass(
    Class = "CytoImageList",
    package = "CytoImageList",
    slots = c(int_metadata="list",
              channelData="DataFrame"),
    contains = "SimpleList",
    prototype = prototype(
    int_metadata = list(
        version = packageVersion("CytoImageList")),
    elementType = "Image_OR_DelayedArray"
    )
)

# Validity checks
#' @importFrom S4Vectors setValidity2
#' @importFrom EBImage colorMode
S4Vectors:::setValidity2(Class = "CytoImageList", .ImageList_validity)

.ImageList_validity <- function(object) {

    msg <- NULL

    # Check if all images have the same number of channels
    dims <- unlist(lapply(object, function(x){
        dim(x)[3]
    }))
    
    if (length(unique(dims)) > 1L) {
        msg <- c(msg, "The images contain different number of channels.\n")
    }

    # Check if all channels have the same names
    if (length(dim(object[[1]])) == 3L) {
        
        cur_names <- dimnames(object[[1]])[[3]]
        errors <- unlist(lapply(object, function(x){
            !identical(cur_names, dimnames(x)[[3]])
        }))
        
        if (sum(errors) > 0) {
            msg <- c(msg, "Not all channels have the same names.\n")
        }
    }

    # Check if entry names are unique
    if (!is.null(names(object)) &&
        length(unique(names(object))) < length(names(object))) {
        msg <- c(msg, "Only unique entries allowed in a CytoImageList object.\n")
    }

    # Check if names contain NA or empties
    if (!is.null(names(object)) && (sum(is.na(names(object))) > 0 ||
            sum(names(object) %in% "") > 0)) {
        msg <- c(msg, "Empty or NA names not supported.\n")
    }

    # Check if channelNames are unique
    if (!is.null(channelNames(object)) &&
        length(unique(channelNames(object))) < length(channelNames(object))) {
        msg <- c(msg, paste("Only unique channels",
                            "allowed in a CytoImageList object.\n"))
    }
    
    # Check if channelNames and rownames(channelData) match
    if (!is.null(channelNames(object)) && 
        !identical(channelNames(object), rownames(channelData(object)))) {
        msg <- c(msg, paste("'channelNames' and 'channelData' do not match.\n"))
    }
    
    # Check if int_metadata contains the version slot
    if (!is(int_metadata(object), "list")) {
        msg <- c(msg, paste("'int_metadata' needs to be a list.\n"))
    }
    if (!"version" %in% names(int_metadata(object))) {
        msg <- c(msg, paste("The 'version' entry needs to be present in 'int_metadata'."))
    }

    if (length(msg)) { return(msg) }

    return(TRUE)
}





