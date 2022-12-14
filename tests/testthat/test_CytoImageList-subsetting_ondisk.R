test_that("On disk: Coercion works on CytoImageList object.", {
    CIL <- exampleCIL()
    
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
    
  cur_Images <- CytoImageList(CIL, on_disk = TRUE, h5FilesPath = cur_path)
    
  cur_size <- file.info(paste0(cur_path, "/image1.h5"))[,"size"]
    
  expect_silent(test.list1 <- as.list(cur_Images))
  expect_s4_class(test.list1[[1]], "HDF5Array")

  expect_silent(test.list2 <- as(cur_Images, "List"))
  expect_identical(mcols(test.list2), mcols(cur_Images))
  expect_identical(test.list2[[1]], cur_Images[[1]])
  expect_s4_class(test.list2[[1]], "HDF5Array")

  expect_silent(test.list3 <- as(cur_Images, "SimpleList"))
  expect_identical(mcols(test.list3), mcols(cur_Images))
  expect_identical(test.list3[[1]], cur_Images[[1]])
  expect_s4_class(test.list3[[1]], "HDF5Array")

  expect_silent(test.list <- as(test.list1, "CytoImageList"))
  expect_true(is.null(mcols(test.list)))
  expect_s4_class(test.list[[1]], "HDF5Array")

  expect_silent(test.list <- as(test.list2, "CytoImageList"))
  expect_identical(mcols(test.list2), mcols(test.list))
  expect_identical(test.list2[[1]], test.list[[1]])

  expect_silent(test.list <- as(test.list3, "CytoImageList"))
  expect_identical(mcols(test.list3), mcols(test.list))
  expect_identical(test.list3[[1]], test.list[[1]])
  
  expect_identical(cur_size, file.info(paste0(cur_path, "/image1.h5"))[,"size"])
})

test_that("On disk: Merging works on CytoImageList object.", {
  CIL <- exampleCIL()
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(CIL, on_disk = TRUE, h5FilesPath = cur_path)
  
  cur_size <- file.info(paste0(cur_path, "/image1.h5"))[,"size"]
    
  # Merging
  ## Should work
  expect_silent(test.list <- c(cur_Images[c(1,3)], cur_Images[2]))
  expect_s4_class(test.list, "CytoImageList")
  expect_equal(names(test.list), c("image1", "image3", "image2"))
  expect_equal(rownames(mcols(test.list)), c("image1", "image3", "image2"))
  expect_identical(test.list[[3]], cur_Images[[2]])

  ## Should fail due to duplicated names
  expect_error(test.list <- c(cur_Images, cur_Images),
               regexp = "Only unique entries allowed in a CytoImageList object.",
               fixed = TRUE)

  # Merge channels
  channels1 <- getChannels(cur_Images, 1:2)
  channels2 <- getChannels(cur_Images, 3:4)
  
  # Error
  expect_error(channels3 <- mergeChannels(channels1, channels2),
               regexp = "Please specify the filepath \nwhere the merged images should be stored.",
               fixed = TRUE)

  ## Should work
  dir.create(file.path(cur_path, "test"))
  cur_path_2 <- file.path(cur_path, "test")
  on.exit(unlink(cur_path_2))
  expect_silent(channels3 <- mergeChannels(channels1, channels2, 
                                           h5FilesPath = cur_path_2))
  expect_equal(channelNames(channels3), c("ch1", "ch2", "ch3", "ch4"))
  expect_equal(names(channels3), c("image1", "image2", "image3"))
  
  expect_true(expect_true(file.exists(file.path(cur_path_2, "image1.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path_2, "image2.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path_2, "image3.h5"))))

  expect_lt(file.info(paste0(cur_path_2, "/image1.h5"))[,"size"],
            file.info(paste0(cur_path, "/image1.h5"))[,"size"])
  
  # Overwrite old files
  expect_silent(channels3 <- mergeChannels(channels1, channels2, 
                                           h5FilesPath = cur_path))
  expect_lt(file.info(paste0(cur_path, "/image1.h5"))[,"size"],
            cur_size)
  
  ## Should not work
  cur_Images <- CytoImageList(CIL, on_disk = TRUE, h5FilesPath = cur_path)

  channels1 <- getChannels(cur_Images, 1:2)
  channels2 <- getChannels(cur_Images, 3:4)
  expect_error(mergeChannels(channels1, as(channels2, "SimpleList")))

  channels2 <- channels2[1:2]
  expect_error(mergeChannels(channels1, channels2))
  expect_error(mergeChannels(channels1, channels2[[1]]))
  expect_error(mergeChannels(channels1, as(channels2)))

  expect_error(mergeChannels(channels1, channels1))
  
  expect_identical(cur_size, file.info(paste0(cur_path, "/image1.h5"))[,"size"])
})

test_that("On disk: General operations work on CytoImageList object.", {
  CIL <- exampleCIL()
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
    
  cur_Images <- CytoImageList(CIL, on_disk = TRUE, h5FilesPath = cur_path)
    
  cur_size <- file.info(paste0(cur_path, "/image1.h5"))[,"size"]
    
  # Subsetting
  ## Getters
  expect_true(is(cur_Images[1], "CytoImageList"))
  expect_true(is(cur_Images[[1]], "HDF5Array"))

  expect_equal(names(cur_Images), c("image1", "image2", "image3"))
  expect_equal(rownames(mcols(cur_Images)), c("image1", "image2", "image3"))

  ## Setters
  #### These checks are mainly in place to avoid NA or empty names
  cur_Images_2 <- cur_Images
  names(cur_Images_2) <- NULL

  ### Should fail
  expect_error(cur_Images_2["test"] <- cur_Images[1])
  expect_error(names(cur_Images_2) <- c("test1", "test2"))
  expect_error(cur_Images_2[1] <- cur_Images[[1]])

  ### Should work
  expect_silent(cur_Images_2[1] <- cur_Images[1])
  expect_silent(cur_Images_2[1] <- as(list(cur_Images[[2]]), "CytoImageList"))

  names(cur_Images_2) <- c("test1", "test2", "test3")
  expect_equal(names(cur_Images_2), c("test1", "test2", "test3"))
  expect_equal(rownames(mcols(cur_Images_2)), c("test1", "test2", "test3"))

  expect_error(cur_Images_2[1] <- "test")
  expect_error(cur_Images_2[[1]] <- "test")

  ### Test mcols
  cur_Images1 <- cur_Images
  cur_Images2 <- cur_Images
  names(cur_Images2) <- c("test1", "test2", "test3")
  mcols(cur_Images2)$ImageNumber <- mcols(cur_Images2)$ImageNb

  cur_Images3 <- c(cur_Images1, cur_Images2)
  expect_true(all(is.na(mcols(cur_Images3[1:3,3]))))

  ### Test channel subsetting
  cur_Images_2 <- cur_Images
  expect_error(cur_Images_2[1] <- as(list(cur_Images_2[[1]][,,1]), "CytoImageList"))

  ## Looping
  ### Should work
  expect_silent(cur_list <- lapply(cur_Images, dim))
  expect_true(is.list(cur_list))
  expect_silent(cur_list <- endoapply(cur_Images, function(x){
    Image(EBImage::gblur(x, sigma = 1))}))
  expect_true(is(cur_list, "CytoImageList"))

  ### Should not work
  expect_error(cur_list <- endoapply(cur_Images, dim))

  ## Vector functions
  expect_equal(length(cur_Images), 3L)
  expect_null(dim(cur_Images))
})

test_that("On disk: Custom accessors work on CytoImageList object.", {
    CIL <- exampleCIL()
    
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
    
  cur_Images <- CytoImageList(CIL, on_disk = TRUE, h5FilesPath = cur_path)
    
  cur_size <- file.info(paste0(cur_path, "/image1.h5"))[,"size"]
  
  # Accessors
  ## getImages
  ### Should work
  expect_s4_class(getImages(cur_Images, "image1"), "CytoImageList")
  expect_s4_class(getImages(cur_Images, "image1")[[1]], "HDF5Array")
  expect_s4_class(getImages(cur_Images, 1), "CytoImageList")
  expect_s4_class(getImages(cur_Images, 1)[[1]], "HDF5Array")
  expect_s4_class(getImages(cur_Images, c(TRUE, FALSE, FALSE)), "CytoImageList")
  expect_s4_class(getImages(cur_Images, c(TRUE, FALSE, FALSE))[[1]], "HDF5Array")
  expect_s4_class(getImages(cur_Images, c("image1", "image2")), "CytoImageList")
  expect_s4_class(getImages(cur_Images, c(1,2)), "CytoImageList")

  ### Should not work
  expect_error(getImages(cur_Images, "A"))
  expect_error(getImages(cur_Images, "image1", "test"))
  expect_error(getImages(cur_Images, c("image1", "test")))
  expect_error(getImages(cur_Images, 4))

  ## setImages
  cur_Images1 <- cur_Images
  cur_Images2 <- cur_Images
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
  expect_s4_class(getChannels(cur_Images, 1), "CytoImageList")
  expect_s4_class(getChannels(cur_Images, 1:2), "CytoImageList")
  expect_s4_class(getChannels(cur_Images, "ch1"), "CytoImageList")

  expect_silent(test <- getChannels(cur_Images, 1))
  expect_equal(channelNames(test), "ch1")
  expect_equal(length(test), 3L)
  expect_equal(names(test), c("image1", "image2", "image3"))

  ### Should not work
  expect_error(getChannels(cur_Images, 10))
  expect_error(getChannels(cur_Images, "test"))
  expect_error(getChannels(cur_Images, c("ch1", "test")))

  ## setChannels
  cur_Images1 <- cur_Images
  cur_Images2 <- getChannels(cur_Images, 2)
  channelNames(cur_Images2) <- "test"

  ### Should work
  expect_silent(setChannels(cur_Images1, 1) <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("test", "ch2", "ch3", "ch4", "ch5"))
  expect_equal(as.matrix(cur_Images1[[1]][,,1]), as.matrix(cur_Images[[1]][,,2]))

  expect_silent(setChannels(cur_Images1, "ch3") <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("test", "ch2", "ch3", "ch4", "ch5"))
  expect_equal(as.matrix(cur_Images1[[1]][,,2]), as.matrix(cur_Images1[[1]][,,3]))

  cur_Images1 <- cur_Images
  cur_Images2 <- getChannels(cur_Images, 2:3)
  channelNames(cur_Images2) <- c("test1", "test2")

  expect_silent(setChannels(cur_Images1, 1:2) <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("test1", "test2", "ch3", "ch4", "ch5"))
  expect_equal(as.matrix(cur_Images1[[1]][,,2]), as.matrix(cur_Images1[[1]][,,3]))

  expect_silent(setChannels(cur_Images1, 1:2) <- NULL)
  expect_equal(channelNames(cur_Images1), c("ch3", "ch4", "ch5"))
  expect_silent(setChannels(cur_Images1, "ch3") <- NULL)
  expect_equal(channelNames(cur_Images1), c("ch4", "ch5"))

  ### Should not work
  cur_Images1 <- cur_Images
  cur_Images2 <- getChannels(cur_Images, 2)
  expect_error(setChannels(cur_Images1, 6) <- cur_Images2)
  expect_error(setChannels(cur_Images1, "test") <- cur_Images2)
  expect_error(setChannels(cur_Images1, 1) <- "test")
  expect_error(setChannels(cur_Images1, 1) <- cur_Images2)
})

