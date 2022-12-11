test_that("On disk: Images can be loaded into CytoImageList object.", {
  library(S4Vectors)
  CIL <- exampleCIL()
    
  files <- list.files(system.file("extdata", package = "CytoImageList"),
             pattern = "imc.tiff", full.names = TRUE)

  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  # Should work - reading in on disk
  cur_list <- lapply(files, readImage)  
  names(cur_list) <- c("image1", "image2", "image3")
  expect_silent(cur_ImageList <- CytoImageList(cur_list, on_disk = TRUE, 
                                               h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  
  expect_s4_class(cur_ImageList$image1, "HDF5Array")
  expect_equal(cur_ImageList$image1@seed@name, "/image1")
  expect_false(cur_ImageList$image1@seed@as_sparse)
  expect_equal(cur_ImageList$image1@seed@dim, c(100, 100, 5))
  expect_equal(cur_ImageList$image1@seed@first_val, 2.235787, tolerance = 0.00001)
  
  expect_true(expect_true(file.exists(file.path(cur_path, "image1.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "image2.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "image3.h5"))))
  
  expect_equal(names(cur_ImageList), c("image1", "image2", "image3"))
  
  expect_identical(as.array(cur_ImageList$image1)[1:10, 1:10, 1], 
                   as.array(CIL$image1)[1:10, 1:10, 1])
  
  expect_true(file.remove(file.path(cur_path, "image1.h5")))
  expect_true(file.remove(file.path(cur_path, "image2.h5")))
  expect_true(file.remove(file.path(cur_path, "image3.h5")))
  
  expect_silent(cur_ImageList <- CytoImageList(test1 = cur_list[[1]],
                                           test2 = cur_list[[1]],
                                           on_disk = TRUE, 
                                           h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(length(cur_ImageList), 2L)
  
  expect_s4_class(cur_ImageList$test1, "HDF5Array")
  expect_equal(cur_ImageList$test1@seed@name, "/test1")
  expect_false(cur_ImageList$test1@seed@as_sparse)
  expect_equal(cur_ImageList$test1@seed@dim, c(100, 100, 5))
  expect_equal(cur_ImageList$test1@seed@first_val, 2.235787, tolerance = 0.00001)
  
  expect_true(file.exists(file.path(cur_path, "test1.h5")))
  expect_true(file.exists(file.path(cur_path, "test2.h5")))
  
  expect_equal(names(cur_ImageList), c("test1", "test2"))
  
  expect_true(file.remove(file.path(cur_path, "test1.h5")))
  expect_true(file.remove(file.path(cur_path, "test2.h5")))
  
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "SimpleList"),
                                               on_disk = TRUE, 
                                               h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(names(cur_ImageList), c("image1", "image2", "image3"))
  expect_true(file.remove(file.path(cur_path, "image1.h5")))
  expect_true(file.remove(file.path(cur_path, "image2.h5")))
  expect_true(file.remove(file.path(cur_path, "image3.h5")))
  
  
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "List"),
                                               on_disk = TRUE, 
                                               h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(names(cur_ImageList), c("image1", "image2", "image3"))
  expect_true(file.remove(file.path(cur_path, "image1.h5")))
  expect_true(file.remove(file.path(cur_path, "image2.h5")))
  expect_true(file.remove(file.path(cur_path, "image3.h5")))
  
  ## Parallelisation
  cur_list <- lapply(files, readImage)  
  names(cur_list) <- c("image1", "image2", "image3")
  expect_silent(cur_ImageList <- CytoImageList(cur_list, on_disk = TRUE, 
                                               h5FilesPath = cur_path,
                                               BPPARAM = BiocParallel::bpparam()))
  
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_true(file.remove(file.path(cur_path, "image1.h5")))
  expect_true(file.remove(file.path(cur_path, "image2.h5")))
  expect_true(file.remove(file.path(cur_path, "image3.h5")))
  
  # Should work - memory --> on disk
  expect_silent(cur_ImageList <- CytoImageList(CIL, on_disk = TRUE, 
                                               h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(mcols(cur_ImageList), mcols(CIL))
  expect_equal(channelNames(cur_ImageList), channelNames(CIL))
  expect_equal(names(cur_ImageList), names(CIL))
  expect_equal(as.array(cur_ImageList$image1), as.array(CIL$image1))
  expect_lt(object.size(cur_ImageList), object.size(CIL))
  
  expect_s4_class(cur_ImageList$image1, "HDF5Array")
  expect_equal(cur_ImageList$image1@seed@name, "/image1")
  expect_false(cur_ImageList$image1@seed@as_sparse)
  expect_equal(cur_ImageList$image1@seed@dim, c(100, 100, 5))
  expect_equal(cur_ImageList$image1@seed@first_val, 2.235787, tolerance = 0.00001)
  
  expect_true(expect_true(file.exists(file.path(cur_path, "image1.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "image2.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "image3.h5"))))
  
  expect_true(file.remove(file.path(cur_path, "image1.h5")))
  expect_true(file.remove(file.path(cur_path, "image2.h5")))
  expect_true(file.remove(file.path(cur_path, "image3.h5")))
  
  ## Parallelisation
  expect_silent(cur_ImageList <- CytoImageList(CIL, on_disk = TRUE, 
                                               h5FilesPath = cur_path,
                                               BPPARAM = BiocParallel::bpparam()))
  
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(mcols(cur_ImageList), mcols(CIL))
  expect_equal(channelNames(cur_ImageList), channelNames(CIL))
  expect_equal(names(cur_ImageList), names(CIL))
  expect_equal(as.array(cur_ImageList$image1), as.array(CIL$image1))
  expect_lt(object.size(cur_ImageList), object.size(CIL))
  
  expect_s4_class(cur_ImageList$image1, "HDF5Array")
  expect_equal(cur_ImageList$image1@seed@name, "/image1")
  expect_false(cur_ImageList$image1@seed@as_sparse)
  expect_equal(cur_ImageList$image1@seed@dim, c(100, 100, 5))
  expect_equal(cur_ImageList$image1@seed@first_val, 2.235787, tolerance = 0.00001)
  
  expect_true(expect_true(file.exists(file.path(cur_path, "image1.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "image2.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "image3.h5"))))
  
  # Should work - on disk --> memory
  expect_silent(cur_ImageList_memory <- CytoImageList(cur_ImageList, on_disk = FALSE))
  expect_equal(cur_ImageList_memory$image1, CIL$image1)
  expect_equal(mcols(cur_ImageList_memory), mcols(CIL))
  expect_equal(channelNames(cur_ImageList_memory), channelNames(CIL))
  expect_equal(names(cur_ImageList_memory), names(CIL))
  expect_gt(object.size(cur_ImageList_memory), object.size(cur_ImageList))
  
  ## Parallelisation
  expect_silent(cur_ImageList_memory <- CytoImageList(cur_ImageList, on_disk = FALSE,
                                                      BPPARAM = BiocParallel::bpparam()))
  
  expect_equal(cur_ImageList_memory$image1, CIL$image1)
  expect_equal(mcols(cur_ImageList_memory), mcols(CIL))
  expect_equal(channelNames(cur_ImageList_memory), channelNames(CIL))
  expect_equal(names(cur_ImageList_memory), names(CIL))
  expect_gt(object.size(cur_ImageList_memory), object.size(cur_ImageList))
  
  expect_true(file.remove(file.path(cur_path, "image1.h5")))
  expect_true(file.remove(file.path(cur_path, "image2.h5")))
  expect_true(file.remove(file.path(cur_path, "image3.h5")))

  # Should not work
  expect_silent(cur_ImageList <- CytoImageList(CIL, on_disk = TRUE, 
                                               h5FilesPath = cur_path))                                             
  cur_list <- as.list(cur_ImageList)
  names(cur_list) <- NULL
  
  expect_error(CytoImageList(cur_list, on_disk = FALSE),
               regexp = paste0("Please specify the names of the images"),
               fixed = TRUE)
  
  cur_list <- lapply(files, readImage)
  expect_error(cur_ImageList <- CytoImageList(cur_list, on_disk = TRUE),
               regexp = paste0("Please specify the names of the images"),
               fixed = TRUE)
  names(cur_list) <- c("image1", "image2", "image3")
  expect_error(cur_ImageList <- CytoImageList(cur_list, on_disk = TRUE),
               regexp = paste0("When storing the images on disk, please specify a 'h5FilesPath'. \n",
                               "You can use 'h5FilesPath = getHDF5DumpDir()' to temporarily store the images.\n",
                               "If doing so, .h5 files will be deleted once the R session ends."),
               fixed = TRUE)

  cur_list[[1]] <- cur_list[[1]][,,-1]
  expect_error(CytoImageList(cur_list, on_disk = TRUE, h5FilesPath = cur_path))
})

test_that("On disk: Show function works.", {
    CIL <- exampleCIL()
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    expect_silent(cur_ImageList <- CytoImageList(CIL, on_disk = TRUE, 
                                                 h5FilesPath = cur_path))
    
    test <- capture.output(show(cur_ImageList))
    expect_length(test, 4)
    expect_equal(test[1], "CytoImageList containing 3 image(s)")
    expect_equal(test[2], "names(3): image1 image2 image3 ")
    expect_equal(test[3], "Each image contains 5 channel(s)")
    expect_equal(test[4], "channelNames(5): ch1 ch2 ch3 ch4 ch5 ")
})
