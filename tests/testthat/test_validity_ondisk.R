test_that("On disk: Image setting validity check is correct.", {
    library(cytomapper)
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
    # Error
    expect_error(.valid.Image.setting(value = "test"), 
                 regexp = "Invalid replacement operation: \nOnly 'Image' or 'CytoImageList' objects allowed.",
                 fixed = TRUE)
    cur_x <- cur_Images
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = cur_Images[1]), 
                 regexp = "Invalid replacement operation: \nCannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- cur_Images
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = cur_Images[[1]]), 
                 regexp = "Invalid replacement operation: \nCannot set Image object to named CytoImageList.",
                 fixed = TRUE)
    cur_value <- cur_Images
    names(cur_value) <- NULL
    expect_error(.valid.Image.setting(x = cur_Images, i = 1, value = cur_value[1]), 
                 regexp = "Invalid replacement operation: \nCannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- cur_Images
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = "test", value = cur_Images[1]), 
                 regexp = "Invalid replacement operation: \n'i' is of type character. \n This setting is only allowed for named CytoImageList objects.",
                 fixed = TRUE)
})