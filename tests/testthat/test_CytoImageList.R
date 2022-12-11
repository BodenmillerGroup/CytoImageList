test_that("Images can be loaded into CytoImageList object.", {
  files <- list.files(system.file("extdata", package = "CytoImageList"),
             pattern = "imc.tiff", full.names = TRUE)

  # Should work
  cur_list <- lapply(files, readImage)
  expect_silent(cur_ImageList <- CytoImageList(cur_list))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_silent(cur_ImageList <- CytoImageList(test1 = cur_list[[1]],
                                           test2 = cur_list[[1]]))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "SimpleList")))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "List")))
  expect_s4_class(cur_ImageList, "CytoImageList")

  # Should not work
  expect_error(CytoImageList("test"))

  cur_list[[1]] <- cur_list[[1]][,,-1]
  expect_error(CytoImageList(cur_list))
})

test_that("Show function works.", {
    CIL <- exampleCIL()
    
    test <- capture.output(show(CIL))
    expect_length(test, 4)
    expect_equal(test[1], "CytoImageList containing 3 image(s)")
    expect_equal(test[2], "names(3): image1 image2 image3 ")
    expect_equal(test[3], "Each image contains 5 channel(s)")
    expect_equal(test[4], "channelNames(5): ch1 ch2 ch3 ch4 ch5 ")
})
