test_that("Coercion works on CytoImageList object.", {
    library(S4Vectors)
    
    CIL <- exampleCIL()

    expect_silent(test.list1 <- as.list(CIL))

    expect_silent(test.list2 <- as(CIL, "List"))
    expect_identical(mcols(test.list2), mcols(CIL))
    expect_identical(test.list2[[1]], CIL[[1]])

    expect_silent(test.list3 <- as(CIL, "SimpleList"))
    expect_identical(mcols(test.list3), mcols(CIL))
    expect_identical(test.list3[[1]], CIL[[1]])

    expect_silent(test.list <- as(test.list1, "CytoImageList"))
    expect_true(is.null(mcols(test.list)))

    expect_silent(test.list <- as(test.list2, "CytoImageList"))
    expect_identical(mcols(test.list2), mcols(test.list))
    expect_identical(test.list2[[1]], test.list[[1]])

    expect_silent(test.list <- as(test.list3, "CytoImageList"))
    expect_identical(mcols(test.list3), mcols(test.list))
    expect_identical(test.list3[[1]], test.list[[1]])
})

test_that("Merging works on CytoImageList object.", {
    CIL <- exampleCIL()
    
  # Merging
  ## Should work
  expect_silent(test.list <- c(CIL[c(1,3)], CIL[2]))
  expect_s4_class(test.list, "CytoImageList")
  expect_equal(names(test.list), c("image1", "image3", "image2"))
  expect_equal(rownames(mcols(test.list)), c("image1", "image3", "image2"))
  expect_identical(test.list[[3]], CIL[[2]])

  ## Should fail due to duplicated names
  expect_error(test.list <- c(CIL, CIL),
               regexp = "Only unique entries allowed in a CytoImageList object.",
               fixed = TRUE)

  # Merge channels
  channels1 <- getChannels(CIL, 1:2)
  channels2 <- getChannels(CIL, 3:4)

  ## Should work
  expect_silent(channels3 <- mergeChannels(channels1, channels2))
  expect_equal(channelNames(channels3), c("ch1", "ch2", "ch3", "ch4"))
  expect_equal(names(channels3), c("image1", "image2", "image3"))
  
  # Check if mcols are correctly set
  mcols(channels1) <- DataFrame(test = c("test1", "test2", "test3"))
  mcols(channels2) <- DataFrame(test2 = c("test5", "test6", "test7"))
  expect_silent(channels3 <- mergeChannels(channels1, channels2))
  expect_equal(mcols(channels3), mcols(channels1))

  ## Should not work
  channels1 <- getChannels(CIL, 1:2)
  channels2 <- getChannels(CIL, 3:4)
  expect_error(mergeChannels(channels1, as(channels2, "SimpleList")))

  channels2 <- channels2[1:2]
  expect_error(mergeChannels(channels1, channels2))
  expect_error(mergeChannels(channels1, channels2[[1]]))
  expect_error(mergeChannels(channels1, as(channels2)))

  expect_error(mergeChannels(channels1, channels1))
})

test_that("General operations work on CytoImageList object.", {
    CIL <- exampleCIL()
    
  # Subsetting
  ## Getters
  expect_true(is(CIL[1], "CytoImageList"))
  expect_true(is(CIL[[1]], "Image"))

  expect_equal(names(CIL), c("image1", "image2", "image3"))
  expect_equal(rownames(mcols(CIL)), c("image1", "image2", "image3"))

  ## Setters
  #### These checks are mainly in place to avoid NA or empty names
  cur_Images <- CIL
  names(cur_Images) <- NULL

  ### Should fail
  expect_error(cur_Images["test"] <- CIL[1])
  expect_error(names(cur_Images) <- c("test1", "test2"))
  expect_error(cur_Images[1] <- CIL[[1]])

  ### Should work
  expect_silent(cur_Images[1] <- CIL[1])
  expect_silent(cur_Images[1] <- as(CIL[[2]], "CytoImageList"))

  names(cur_Images) <- c("test1", "test2", "test3")
  expect_equal(names(cur_Images), c("test1", "test2", "test3"))
  expect_equal(rownames(mcols(cur_Images)), c("test1", "test2", "test3"))

  expect_error(cur_Images[1] <- "test")
  expect_error(cur_Images[[1]] <- "test")

  ### Test mcols
  cur_Images1 <- CIL
  cur_Images2 <- CIL
  names(cur_Images2) <- c("test1", "test2", "test3")
  mcols(cur_Images2)$ImageNumber <- mcols(cur_Images2)$ImageNb

  cur_Images3 <- c(cur_Images1, cur_Images2)
  expect_true(all(is.na(mcols(cur_Images3[1:3,3]))))

  ### Test channel subsetting
  cur_Images <- CIL
  expect_error(cur_Images[1] <- as(cur_Images[[1]][,,1], "CytoImageList"))

  ## Looping
  ### Should work
  expect_silent(cur_list <- lapply(CIL, dim))
  expect_true(is.list(cur_list))
  expect_silent(cur_list <- endoapply(CIL, function(x){
    EBImage::gblur(x, sigma = 1)
    return(x)}))
  expect_true(is(cur_list, "CytoImageList"))

  ### Should not work
  expect_error(cur_list <- endoapply(CIL, dim))

  ## Vector functions
  expect_equal(length(CIL), 3L)
  expect_null(dim(CIL))
})

test_that("Custom accessors work on CytoImageList object.", {
    CIL <- exampleCIL()
    
  # Accessors
  ## getImages
  ### Should work
  expect_s4_class(getImages(CIL, "image1"), "CytoImageList")
  expect_s4_class(getImages(CIL, "image1")[[1]], "Image")
  expect_s4_class(getImages(CIL, 1), "CytoImageList")
  expect_s4_class(getImages(CIL, 1)[[1]], "Image")
  expect_s4_class(getImages(CIL, c(TRUE, FALSE, FALSE)), "CytoImageList")
  expect_s4_class(getImages(CIL, c(TRUE, FALSE, FALSE))[[1]], "Image")
  expect_s4_class(getImages(CIL, c("image1", "image2")), "CytoImageList")
  expect_s4_class(getImages(CIL, c(1,2)), "CytoImageList")

  ### Should not work
  expect_error(getImages(CIL, "A"))
  expect_error(getImages(CIL, "image1", "test"))
  expect_error(getImages(CIL, c("image1", "test")))
  expect_error(getImages(CIL, 4))

  ## setImages
  cur_Images1 <- CIL
  cur_Images2 <- CIL
  names(cur_Images2) <- c("test1", "test2", "test3")
  mcols(cur_Images2)$ImageNumber <- mcols(cur_Images2)$ImageNb
  cur_Images3 <- cur_Images2
  names(cur_Images3) <- NULL

  ### Should work
  expect_silent(setImages(cur_Images1, "image1") <- cur_Images2[1])
  expect_equal(names(cur_Images1), c("image1", "image2", "image3"))
  expect_equal(channelNames(cur_Images1), c("ch1", "ch2", "ch3", "ch4", "ch5"))
  expect_equal(mcols(cur_Images1)$ImageNb, c(1, 2, 3))
  expect_equal(mcols(cur_Images1)$ImageNumber, c(1, NA, NA))
  expect_silent(setImages(cur_Images1, "image1") <- cur_Images2[[1]])
  expect_equal(names(cur_Images1), c("image1", "image2", "image3"))
  expect_equal(channelNames(cur_Images1), c("ch1", "ch2", "ch3", "ch4", "ch5"))
  expect_equal(mcols(cur_Images1)$ImageNb, c(1, 2, 3))
  expect_equal(mcols(cur_Images1)$ImageNumber, c(1, NA, NA))
  expect_silent(setImages(cur_Images1, "image4") <- cur_Images2[1])
  expect_equal(names(cur_Images1), c("image1", "image2", "image3", "image4"))
  expect_equal(channelNames(cur_Images1), c("ch1", "ch2", "ch3", "ch4", "ch5"))
  expect_silent(setImages(cur_Images1, 1) <- cur_Images2[2])
  expect_equal(names(cur_Images1), c("test2", "image2", "image3", "image4"))
  expect_equal(channelNames(cur_Images1), c("ch1", "ch2", "ch3", "ch4", "ch5"))
  expect_silent(setImages(cur_Images1, 1:2) <- cur_Images2[2:3])
  expect_equal(names(cur_Images1), c("test2", "test3", "image3", "image4"))
  expect_equal(channelNames(cur_Images1), c("ch1", "ch2", "ch3", "ch4", "ch5"))
  expect_silent(setImages(cur_Images3, 1) <- cur_Images3[2])
  expect_equal(channelNames(cur_Images3), c("ch1", "ch2", "ch3", "ch4", "ch5"))

  ### Should not work
  expect_error(setImages(cur_Images1, 1) <- cur_Images2[[1]])
  expect_error(setImages(cur_Images1, 1:2) <- cur_Images2[2])
  expect_error(setImages(cur_Images1, 1) <- cur_Images3[2])
  expect_error(setImages(cur_Images3, 1) <- CIL[2])
  expect_error(setImages(cur_Images3, "test") <- cur_Images3[2])

  ### Remove images
  expect_silent(setImages(cur_Images1, 1) <- NULL)
  expect_equal(names(cur_Images1), c("test3", "image3", "image4"))
  expect_silent(setImages(cur_Images1, "test3") <- NULL)
  expect_equal(names(cur_Images1), c("image3", "image4"))

  ## getChannels
  ### Should work
  expect_s4_class(getChannels(CIL, 1), "CytoImageList")
  expect_s4_class(getChannels(CIL, 1:2), "CytoImageList")
  expect_s4_class(getChannels(CIL, "ch1"), "CytoImageList")

  expect_silent(test <- getChannels(CIL, 1))
  expect_equal(channelNames(test), "ch1")
  expect_equal(length(test), 3L)
  expect_equal(names(test), c("image1", "image2", "image3"))

  expect_silent(test <- getChannels(CIL, "ch1"))
  expect_equal(channelNames(test), "ch1")
  expect_equal(length(test), 3L)
  expect_equal(names(test), c("image1", "image2", "image3"))

  ### Should not work
  expect_error(getChannels(CIL, 10))
  expect_error(getChannels(CIL, "test"))
  expect_error(getChannels(CIL, c("ch1", "test")))

  ## setChannels
  cur_Images1 <- CIL
  cur_Images2 <- getChannels(CIL, 2)
  channelNames(cur_Images2) <- "test"

  ### Should work
  expect_silent(setChannels(cur_Images1, 1) <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("test", "ch2", "ch3", "ch4", "ch5"))
  expect_equal(cur_Images1[[1]][,,1], cur_Images1[[1]][,,2])

  expect_silent(setChannels(cur_Images1, "ch3") <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("test", "ch2", "ch3", "ch4", "ch5"))
  expect_equal(cur_Images1[[1]][,,2], cur_Images1[[1]][,,3])

  cur_Images1 <- CIL
  cur_Images2 <- getChannels(CIL, 2:3)
  channelNames(cur_Images2) <- c("test1", "test2")

  expect_silent(setChannels(cur_Images1, 1:2) <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("test1", "test2", "ch3", "ch4", "ch5"))
  expect_equal(cur_Images1[[1]][,,2], cur_Images1[[1]][,,3])

  expect_silent(setChannels(cur_Images1, 1:2) <- NULL)
  expect_equal(channelNames(cur_Images1), c("ch3", "ch4", "ch5"))
  expect_silent(setChannels(cur_Images1, "ch3") <- NULL)
  expect_equal(channelNames(cur_Images1), c("ch4", "ch5"))

  ### Should not work
  cur_Images1 <- CIL
  cur_Images2 <- getChannels(CIL, 2)
  expect_error(setChannels(cur_Images1, 6) <- cur_Images2)
  expect_error(setChannels(cur_Images1, "test") <- cur_Images2)
  expect_error(setChannels(cur_Images1, 1) <- "test")
  expect_error(setChannels(cur_Images1, 1) <- cur_Images2)
})

