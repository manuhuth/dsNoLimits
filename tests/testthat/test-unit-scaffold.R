test_that("the package installs and the DataSHIELD manifest is readable", {
  path <- system.file("DATASHIELD", package = "dsNoLimits")
  expect_true(nzchar(path))
  manifest <- read.dcf(path)
  expect_equal(nrow(manifest), 1L)
  expect_setequal(
    colnames(manifest),
    c("AggregateMethods", "AssignMethods", "Options")
  )
})
