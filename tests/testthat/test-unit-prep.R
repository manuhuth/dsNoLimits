test_that("nolimitsPrepDS validates its arguments before touching Julia", {
  withr::local_options(nlds_fit_options(dsNoLimits.modelDir = NULL,
                                        default.dsNoLimits.modelDir = ""))
  D <- nlds_test_data()

  expect_error(nolimitsPrepDS("D", "a$b", "ID", "t"), "not a plain name")
  expect_error(nolimitsPrepDS("D", strrep("m", 21), "ID", "t"),
               "nfilter.stringShort")
  expect_error(nolimitsPrepDS("D", "onecomp_iv", "ID", "t"),
               "no model registry configured")

  dir <- nlds_model_dir()
  withr::local_options(nlds_fit_options(dsNoLimits.modelDir = dir))
  expect_error(nolimitsPrepDS("D", "absent_model", "ID", "t"),
               "is not a model registered on this server")
})

test_that("nolimitsPrepDS rejects a bad data frame or column name", {
  withr::local_options(nlds_fit_options(dsNoLimits.modelDir = nlds_model_dir()))
  D <- nlds_test_data()
  notadf <- 1:10

  expect_error(nolimitsPrepDS("nosuchobject", "onecomp_iv", "ID", "t"),
               "does not exist on this server")
  expect_error(nolimitsPrepDS("notadf", "onecomp_iv", "ID", "t"),
               "wrong type")
  expect_error(nolimitsPrepDS("D", "onecomp_iv", "ID$x", "t"),
               "not a plain name")
  expect_error(nolimitsPrepDS("D", "onecomp_iv", "SUBJ", "t"),
               "must both be columns of the data frame")
})

test_that("nolimitsPrepDS builds a cache whose metadata is model-derived", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()

  expect_true(.nlds_is_prep(prep))
  expect_equal(prep$p, 4L)
  expect_equal(prep$names, c("cl", "v", "omega", "sigma"))
  # theta0 is the model's declared inits on the transformed scale, never
  # data-derived - that is what makes the client's exact cross-site agreement
  # check sound.
  expect_equal(prep$theta0, c(1, 10, log(0.3), log(0.5)), tolerance = 1e-12)
  expect_length(prep$scale, 4L)
  expect_true(all(is.finite(prep$scale)))
  expect_equal(prep$n.subjects, 6L)
  expect_equal(prep$n.obs, 36L)
  # The canonical model hash: model-derived, and the client's cross-site
  # agreement receipt.
  expect_identical(prep$model.hash, .nlds_model_hash(NLDS_TEST_MODEL))
  expect_match(prep$model.hash, "^[0-9a-f]{64}$")
  expect_equal(prep$estimator.warm, "laplace")
  expect_named(prep$versions,
               c("dsnolimits", "nolimitsr", "nolimits", "julia"))
})

test_that("nolimitsPrepStringDS accepts an encoded model and rejects a bad one", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  D <- nlds_test_data()

  expect_error(nolimitsPrepStringDS("D", "", "ID", "t"),
               "single non-empty character string")
  expect_error(nolimitsPrepStringDS("D", "not base64!", "ID", "t"),
               "not base64url-encoded")
  expect_error(nolimitsPrepStringDS("D", .ds_encode("this is not Julia at all("),
                                    "ID", "t"),
               "could not be built on this server")

  prep <- nolimitsPrepStringDS("D", .ds_encode(NLDS_TEST_MODEL), "ID", "t")
  expect_equal(prep$names, c("cl", "v", "omega", "sigma"))
})

test_that("nolimitsPrepInfoDS returns plain data and never the Julia handle", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()

  res <- nolimitsPrepInfoDS("prep")
  expect_named(res, c("theta0", "names", "scale", "p", "n.subjects", "n.obs",
                      "model.hash", "q1.names", "q2.names", "cf.names",
                      "num.names", "em.available", "versions"))
  expect_false("dm" %in% names(res))
  expect_identical(res$model.hash, .nlds_model_hash(NLDS_TEST_MODEL))
  expect_identical(unserialize(serialize(res, NULL)), res)
  # Nothing whose size depends on n crossed.
  expect_true(all(lengths(res) <= 4L))
})

test_that("nolimitsPrepInfoDS refuses a symbol that is not a fit cache", {
  withr::local_options(nlds_fit_options())
  notaprep <- list(a = 1)
  expect_error(nolimitsPrepInfoDS("notaprep"), "wrong type")
  expect_error(nolimitsPrepInfoDS("nosuchobject"), "does not exist on this server")
})
