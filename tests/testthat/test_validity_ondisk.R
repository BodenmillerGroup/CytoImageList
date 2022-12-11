test_that("On disk: Image setting validity check is correct.", {
    CIL <- exampleCIL()
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(CIL, on_disk = TRUE, h5FilesPath = cur_path)
    
    # Error
    expect_error(.valid.Image.setting(value = "test"), 
                 regexp = "Invalid replacement operation: \nOnly 'Image' or 'CytoImageList' objects allowed.",
                 fixed = TRUE)
    cur_x <- cur_Images
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = cur_Images[1]), 
                 regexp = "Cannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- cur_Images
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = cur_Images[[1]]), 
                 regexp = "Cannot set Image object to named CytoImageList.",
                 fixed = TRUE)
    cur_value <- cur_Images
    names(cur_value) <- NULL
    expect_error(.valid.Image.setting(x = cur_Images, i = 1, value = cur_value[1]), 
                 regexp = "Cannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- cur_Images
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = "test", value = cur_Images[1]), 
                 regexp = "'i' is of type character. \nThis setting is only allowed for named CytoImageList objects.",
                 fixed = TRUE)
})