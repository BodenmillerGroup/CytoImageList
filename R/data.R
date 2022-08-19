#' Example CytoImageList object of image files
#'
#' This \linkS4class{CytoImageList} object contains multi-channel stacks of
#' three images acquired by imaging mass cytometry. Each channel represents the
#' pixel-intensities of each of the 5 measured proteins. The data is part of a
#' imaging mass cytometry study on the progression of Type 1 diabetes and
#' contains pancreas cells.
#'
#' @format A CytoImageList object containing 3 \code{\linkS4class{Image}}
#' objects with 5 channels each. Channel names can be accessed via
#' \code{?\link{channelNames}}.
#'
#' @references
#' \href{https://www.sciencedirect.com/science/article/pii/S1550413118306910}{
#' Damond, N. et al., A Map of Human Type 1 Diabetes Progression by
#' Imaging Mass Cytometry, Cell Metabolism 29:3, 2019}
"pancreasImages"

#' Example CytoImageList object of segmentation masks
#'
#' This \linkS4class{CytoImageList} object contains single-channel images
#' representing the segmentation masks after preprocessing of imaging mass
#' cytometry data. The data is part of a imaging mass cytometry study on the
#' progression of Type 1 diabetes and contains pancreas cells.
#'
#' @format A CytoImageList object containing 3 \code{\linkS4class{Image}}
#' objects with 1 channel each. These images are the result to segmentation
#' and associated to the images stored in the
#' \code{\link[cytomapper]{pancreasImages}} object. Pixel values indicate the
#' numeric cell identifier while a value of 0 represents the image background.
#'
#' @references
#' \href{https://www.sciencedirect.com/science/article/pii/S1550413118306910}{
#' Damond, N. et al., A Map of Human Type 1 Diabetes Progression by
#' Imaging Mass Cytometry, Cell Metabolism 29:3, 2019}
"pancreasMasks"

