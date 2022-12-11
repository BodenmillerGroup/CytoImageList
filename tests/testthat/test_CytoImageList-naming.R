test_that("Image and channel names can be extracted and set.", {
    CIL <- exampleCIL()
    
  cur_Images <- CIL

  # Standard calls - CytoImageList
  expect_equal(channelNames(CIL),
               c("ch1", "ch2", "ch3", "ch4", "ch5"))
  expect_equal(names(CIL),
               c("image1", "image2", "image3"))

  expect_silent(channelNames(cur_Images) <- c("test1", "test2", "test3", "test4", "test5"))
  expect_equal(channelNames(cur_Images),
               c("test1", "test2", "test3", "test4", "test5"))
  expect_silent(names(cur_Images) <- c("test1", "test2", "test3"))
  expect_equal(names(cur_Images),
               c("test1", "test2", "test3"))

  # Should not work
  expect_error(channelNames(cur_Images) <- "test")
  expect_error(names(cur_Images) <- "test")

  # Subset to one channel per image
  cur_Images <- getChannels(CIL, 1)
  expect_equal(channelNames(cur_Images), c("ch1"))
  expect_silent(channelNames(cur_Images) <- "test1")
  expect_equal(channelNames(cur_Images), "test1")

  expect_error(channelNames(cur_Images) <- c("test1", "test2"))

  # Check if expansion works
  ## Subset image that third dimension is lost
  cur_Images <- S4Vectors::endoapply(CIL, function(x){
    return(x[,,1])
  })
  cur_Image2 <- cur_Images
  expect_null(channelNames(cur_Image2))
  channelNames(cur_Image2) <- "test"
  expect_equal(channelNames(cur_Image2), "test")
  expect_identical(cur_Image2[[1]][,,1], cur_Images[[1]])

  # Set null
  expect_silent(channelNames(CIL) <- NULL)
  expect_null(channelNames(CIL))
  expect_silent(names(CIL) <- NULL)
  expect_null(names(CIL))
  })
