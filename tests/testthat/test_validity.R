test_that("Image setting validity check is correct.", {
    data("pancreasImages")
    
    # Error
    expect_error(.valid.Image.setting(value = "test"), 
                 regexp = "Invalid replacement operation: \nOnly 'Image' or 'CytoImageList' objects allowed.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = pancreasImages[1]), 
                 regexp = "Invalid replacement operation: \nCannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = pancreasImages[[1]]), 
                 regexp = "Invalid replacement operation: \nCannot set Image object to named CytoImageList.",
                 fixed = TRUE)
    cur_value <- pancreasImages
    names(cur_value) <- NULL
    expect_error(.valid.Image.setting(x = pancreasImages, i = 1, value = cur_value[1]), 
                 regexp = "Invalid replacement operation: \nCannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = "test", value = pancreasImages[1]), 
                 regexp = "Invalid replacement operation: \n'i' is of type character. \n This setting is only allowed for named CytoImageList objects.",
                 fixed = TRUE)
})

test_that("Channel setting validity check is correct.", {
    # Error
    expect_error(.valid.Channel.setting(value = "test"), 
                 regexp = "Invalid replacement operation: \nOnly 'CytoImageList' objects allowed.\nTo alter Image objects, see ?Image.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    expect_error(.valid.Channel.setting(x = cur_x, value = pancreasImages[1]), 
                 regexp = "Invalid replacement operation: \nReplacement needs to have same length as 'x'.",
                 fixed = TRUE)
    expect_error(.valid.Channel.setting(x = cur_x, i = 1:3, value = pancreasImages), 
                 regexp = "Invalid replacement operation: \nNumber of replacement channels is not the same as \nnumber of channels to replace.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    names(cur_x) <- c("test1", "test2", "test3")
    expect_error(.valid.Channel.setting(x = cur_x, i = 1:3, value = getChannels(pancreasImages, 1:3)), 
                 regexp = "Invalid replacement operation: \nNames of 'x' and 'value' do not match.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    channelNames(cur_x) <- NULL
    expect_error(.valid.Channel.setting(x = cur_x, i = "test", value = getChannels(pancreasImages, 1)), 
                 regexp = "Invalid replacement operation: \nTrying to set a named channel in an unnamed CytoImageList.",
                 fixed = TRUE)
})