test_that("nolimitsStatusDS returns the readiness report and touches no data", {
  withr::local_options(ds_test_options(dsNoLimits.modelDir = NULL,
                                       default.dsNoLimits.modelDir = ""))
  res <- nolimitsStatusDS()

  expect_named(res, c("julia.ok", "julia.version", "nolimits.version",
                      "dsnolimits.version", "nolimitsr.version",
                      "model.dir.set"))
  expect_type(res$julia.ok, "logical")
  expect_false(res$model.dir.set)
  expect_equal(res$dsnolimits.version,
               as.character(utils::packageVersion("dsNoLimits")))
  expect_true(all(vapply(res, length, integer(1)) == 1L))
})

test_that("nolimitsStatusDS reports a configured model directory", {
  withr::local_options(ds_test_options(dsNoLimits.modelDir = tempdir()))
  expect_true(nolimitsStatusDS()$model.dir.set)
})

test_that("nolimitsStatusDS reports Julia when it is provisioned", {
  nlds_skip_no_julia()
  res <- nolimitsStatusDS()
  expect_true(res$julia.ok)
  expect_true(package_version(res$nolimits.version) >= "0.2.6")
})
