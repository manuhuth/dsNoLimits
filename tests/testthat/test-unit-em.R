# MCEM server functions: the E-step assign caches draws, the Q aggregate returns
# a finite per-part value and gradient, and the q1/q2 partition rides prepInfo.
# The guard-when-primitives-absent path cannot be exercised on a server that HAS
# the primitives (the dev env used here does); it is asserted by code review of
# .nlds_em_guard(), which stops with the fixed FAILED string on isdefined FALSE.

test_that("nolimitsEmEStepDS and nolimitsEmQDS validate arguments before Julia", {
  withr::local_options(nlds_fit_options())
  prep <- list(dm = 1, theta0 = rep(0, 4), names = letters[1:4], scale = rep(1, 4),
               p = 4L, n.subjects = 6L, n.obs = 36L, model.hash = "x",
               q1.names = c("cl", "v", "sigma"), q2.names = "omega",
               em.available = TRUE, versions = list())
  th <- .ds_num_encode(rep(0, 4))

  expect_error(nolimitsEmEStepDS("prep", 1, 1L, 1L, 0L),
               "theta must be a single character string")
  expect_error(nolimitsEmEStepDS("prep", .ds_num_encode(rep(0, 3)), 1L, 1L, 0L),
               "must decode to 4 numeric values")
  expect_error(nolimitsEmEStepDS("prep", th, 0L, 1L, 0L),
               "outer.iter must be a whole number")
  expect_error(nolimitsEmEStepDS("prep", th, 1L, 1.5, 0L),
               "base.seed must be a whole number")
  expect_error(nolimitsEmEStepDS("prep", th, 1L, 1L, -1L),
               "site.id must be a whole number")
  expect_error(nolimitsEmEStepDS("prep", th, 1L, 1L, 0L, 0L),
               "sample.schedule must be a whole number")

  expect_error(nolimitsEmQDS("prep", 1, "q1", 1L),
               "theta must be a single character string")
  expect_error(nolimitsEmQDS("prep", th, "q3", 1L),
               "part must be one of q1, q2")
  expect_error(nolimitsEmQDS("prep", th, "q1", 0L),
               "round must be an integer between 1 and 500")
})

test_that("prepInfo carries the MCEM q1/q2 partition", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()

  expect_true(isTRUE(prep$em.available))
  expect_setequal(c(prep$q1.names, prep$q2.names), prep$names)
  expect_true("omega" %in% prep$q2.names)

  info <- nolimitsPrepInfoDS("prep")
  expect_identical(info$q1.names, as.character(prep$q1.names))
  expect_identical(info$q2.names, as.character(prep$q2.names))
  expect_true(info$em.available)
  # Plain vectors survive the serialization boundary unchanged.
  expect_identical(unserialize(serialize(info, NULL)), info)
})

test_that("the E-step caches draws and the Q aggregate is finite per part", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()
  th <- .ds_num_encode(prep$theta0)

  # First outer iteration: fresh prior-mean seeding (state = nothing).
  prep <- nolimitsEmEStepDS("prep", th, 1L, 20260824L, 0L, 20L)
  expect_false(is.null(prep$em))
  expect_identical(prep$em.outer, 1L)

  for (part in c("q1", "q2")) {
    res <- nolimitsEmQDS("prep", th, part, 1L)
    free <- if (part == "q1") prep$q1.names else prep$q2.names
    expect_true(res$finite)
    expect_true(is.finite(res$value))
    expect_length(res$grad, length(free))
    expect_true(all(is.finite(res$grad)))
    expect_identical(res$n.subjects, as.integer(prep$n.subjects))
  }

  # A second outer iteration threads the warm-start state and still evaluates.
  prep <- nolimitsEmEStepDS("prep", th, 2L, 20260824L, 0L, 20L)
  expect_identical(prep$em.outer, 2L)
  res2 <- nolimitsEmQDS("prep", th, "q1", 2L)
  expect_true(res2$finite)

  # A Q call before any E-step fails legibly (self-contained cache, no draws).
  fresh <- list(dm = 1, theta0 = rep(0, 4), names = c("cl", "v", "omega", "sigma"),
                scale = rep(1, 4), p = 4L, n.subjects = 6L, n.obs = 36L,
                model.hash = "x", q1.names = c("cl", "v", "sigma"),
                q2.names = "omega", em.available = TRUE, versions = list())
  expect_error(nolimitsEmQDS("fresh", .ds_num_encode(rep(0, 4)), "q1", 1L),
               "no E-step draws are cached")
})
