test_that("the subject-count gate fires below nfilter.tab and passes at it", {
  nlds_skip_no_julia()
  dir <- nlds_model_dir()
  D <- nlds_test_data()   # 6 subjects

  # Must-fail: one subject above the site's count.
  withr::local_options(nlds_fit_options(nfilter.tab = 7,
                                        dsNoLimits.modelDir = dir))
  expect_error(nolimitsPrepDS("D", "onecomp_iv", "ID", "t"),
               "FAILED: the number of subjects is less than nfilter.tab",
               fixed = TRUE)

  # Negative control: EXACTLY at the threshold must pass, otherwise a guard that
  # fires unconditionally would still satisfy the must-fail case.
  withr::local_options(nlds_fit_options(nfilter.tab = 6,
                                        dsNoLimits.modelDir = dir))
  expect_true(.nlds_is_prep(nolimitsPrepDS("D", "onecomp_iv", "ID", "t")))
})

test_that("the subject-count gate reads the live name first, then default.", {
  dir <- nlds_model_dir()
  D <- nlds_test_data()

  # Live name only.
  opts <- nlds_fit_options(dsNoLimits.modelDir = dir)
  opts$default.nfilter.tab <- NULL
  opts$nfilter.tab <- 7
  withr::local_options(opts)
  expect_error(nolimitsPrepDS("D", "onecomp_iv", "ID", "t"),
               "less than nfilter.tab")

  # default.-prefixed only: the live name unset must not silently pass.
  opts <- nlds_fit_options(dsNoLimits.modelDir = dir)
  opts$nfilter.tab <- NULL
  opts$default.nfilter.tab <- 7
  withr::local_options(opts)
  expect_error(nolimitsPrepDS("D", "onecomp_iv", "ID", "t"),
               "less than nfilter.tab")

  # A live override must win over a permissive default.
  opts$nfilter.tab <- 99
  opts$default.nfilter.tab <- 1
  withr::local_options(opts)
  expect_error(nolimitsPrepDS("D", "onecomp_iv", "ID", "t"),
               "less than nfilter.tab")
})

test_that("a fired subject-count gate returns no payload at all", {
  dir <- nlds_model_dir()
  D <- nlds_test_data()
  withr::local_options(nlds_fit_options(nfilter.tab = 7,
                                        dsNoLimits.modelDir = dir))

  res <- tryCatch(nolimitsPrepDS("D", "onecomp_iv", "ID", "t"),
                  error = function(e) e)
  expect_s3_class(res, "error")
  # The object the client would see is the condition. It must carry the message
  # and nothing derived from the data.
  expect_null(res$call)
  expect_setequal(names(res), c("message", "call"))
  expect_false(grepl("[0-9]", sub("nfilter.tab", "", conditionMessage(res),
                                  fixed = TRUE)))
})

test_that("the saturation gate fires when p exceeds nfilter.glm * n.subjects", {
  nlds_skip_no_julia()
  dir <- nlds_model_dir()
  D <- nlds_test_data()   # 4 parameters over 6 subjects

  # Default nfilter.glm = 0.33 allows 1.98 parameters here.
  withr::local_options(ds_test_options(dsNoLimits.modelDir = dir))
  expect_error(nolimitsPrepDS("D", "onecomp_iv", "ID", "t"),
               "too many parameters for the number of subjects")

  # Negative control: exactly at the threshold (4 <= 0.6667 * 6) passes.
  withr::local_options(ds_test_options(nfilter.glm = 4 / 6,
                                       dsNoLimits.modelDir = dir))
  expect_true(.nlds_is_prep(nolimitsPrepDS("D", "onecomp_iv", "ID", "t")))
})

test_that("an unconfigured threshold is diagnosed rather than silently skipped", {
  dir <- nlds_model_dir()
  D <- nlds_test_data()
  opts <- nlds_fit_options(dsNoLimits.modelDir = dir)
  opts$nfilter.tab <- NULL
  opts$default.nfilter.tab <- NULL
  withr::local_options(opts)

  expect_error(nolimitsPrepDS("D", "onecomp_iv", "ID", "t"),
               "disclosure setting nfilter.tab is not configured")
})
