test_that("nolimitsPrepStringDS is BLOCKED outside permissive mode", {
  dir <- nlds_model_dir()
  D <- nlds_test_data()
  b64 <- .ds_encode(NLDS_TEST_MODEL)

  withr::local_options(nlds_fit_options(control = "non-permissive",
                                        dsNoLimits.modelDir = dir))
  expect_error(
    nolimitsPrepStringDS("D", b64, "ID", "t"),
    "BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked",
    fixed = TRUE)

  # The unset level (the shipped 'banana' default is also not permissive).
  withr::local_options(nlds_fit_options(control = "", dsNoLimits.modelDir = dir))
  expect_error(nolimitsPrepStringDS("D", b64, "ID", "t"), "^BLOCKED: ")
})

test_that("the permissive gate is the FIRST statement: it fires before any validation", {
  D <- nlds_test_data()
  withr::local_options(nlds_fit_options(control = "non-permissive"))
  # Arguments that would each fail their own check still produce BLOCKED.
  expect_error(nolimitsPrepStringDS("nosuchobject", "", "ID$x", 42), "^BLOCKED: ")
})

test_that("a BLOCKED return carries no payload", {
  D <- nlds_test_data()
  withr::local_options(nlds_fit_options(control = "non-permissive"))
  res <- tryCatch(nolimitsPrepStringDS("D", .ds_encode(NLDS_TEST_MODEL), "ID", "t"),
                  error = function(e) e)
  expect_s3_class(res, "error")
  expect_setequal(names(res), c("message", "call"))
  expect_false(any(vapply(c("ID", "conc", "Dose"),
                          function(w) grepl(w, conditionMessage(res), fixed = TRUE),
                          logical(1))))
})

test_that("permissive mode is required only for the string method", {
  nlds_skip_no_julia()
  dir <- nlds_model_dir()
  D <- nlds_test_data()
  withr::local_options(nlds_fit_options(control = "non-permissive",
                                        dsNoLimits.modelDir = dir))
  expect_true(.nlds_is_prep(nolimitsPrepDS("D", "onecomp_iv", "ID", "t")))
})
