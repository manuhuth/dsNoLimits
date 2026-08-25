# SAEM server functions: the stats aggregate returns a fixed-length finite flat
# vector, the coordinator M-step assign updates the cache and the updates
# aggregate reads the transformed-scale closed-form update back, and the
# closed-form / numerical partition rides prepInfo.

test_that("nolimitsSaem*DS validate arguments before Julia", {
  withr::local_options(nlds_fit_options())
  prep <- list(dm = 1, theta0 = rep(0, 4), names = c("cl", "v", "omega", "sigma"),
               scale = rep(1, 4), p = 4L, n.subjects = 6L, n.obs = 36L,
               model.hash = "x", q1.names = c("cl", "v", "sigma"),
               q2.names = "omega", cf.names = c("omega", "sigma"),
               num.names = c("cl", "v"), em.available = TRUE, versions = list())
  th <- .ds_num_encode(rep(0, 4))
  ss <- .ds_num_encode(rep(1, 3))

  expect_error(nolimitsSaemStatsDS("prep", 1, 1L),
               "theta must be a single character string")
  expect_error(nolimitsSaemStatsDS("prep", .ds_num_encode(rep(0, 3)), 1L),
               "must decode to 4 numeric values")
  expect_error(nolimitsSaemStatsDS("prep", th, 0L),
               "round must be an integer between 1 and 500")

  expect_error(nolimitsSaemMstepDS("prep", 1, ss, 1L, 20L),
               "theta must be a single character string")
  expect_error(nolimitsSaemMstepDS("prep", th, 1, 1L, 20L),
               "summed.stats must be a single character string")
  expect_error(nolimitsSaemMstepDS("prep", th, ss, 0L, 20L),
               "outer.iter must be a whole number")
  expect_error(nolimitsSaemMstepDS("prep", th, ss, 1L, 0L),
               "maxiters must be a whole number")
})

test_that("prepInfo carries the SAEM closed-form / numerical partition", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()

  expect_true(isTRUE(prep$em.available))
  expect_setequal(c(prep$cf.names, prep$num.names), prep$names)
  # The log-normal RE scale and the residual sigma are closed-form eligible.
  expect_true("omega" %in% prep$cf.names)
  expect_true(length(intersect(prep$cf.names, prep$num.names)) == 0L)

  info <- nolimitsPrepInfoDS("prep")
  expect_identical(info$cf.names, as.character(prep$cf.names))
  expect_identical(info$num.names, as.character(prep$num.names))
  expect_identical(unserialize(serialize(info, NULL)), info)
})

test_that("stats aggregate, coordinator M-step assign and updates aggregate", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()
  th <- .ds_num_encode(prep$theta0)

  # E-step caches the fixed draws (first outer iteration, fresh seeding).
  prep <- nolimitsEmEStepDS("prep", th, 1L, 20260824L, 0L, 20L)

  # Stats: a fixed-length finite flat vector, independent of subject count.
  st <- nolimitsSaemStatsDS("prep", th, 1L)
  expect_true(is.numeric(st$stats) && length(st$stats) >= 1L)
  expect_true(all(is.finite(st$stats)))
  expect_identical(st$n.subjects, as.integer(prep$n.subjects))
  len1 <- length(st$stats)

  # Coordinator M-step (outer 1, smoothed_state = nothing): assign updates cache.
  summed <- .ds_num_encode(st$stats)
  prep <- nolimitsSaemMstepDS("prep", th, summed, 1L, 20L)
  expect_false(is.null(prep[["saem.update"]]))

  # Updates aggregate reads the transformed-scale closed-form update back.
  upd <- nolimitsSaemUpdatesDS("prep")
  expect_true(is.character(upd$names) && is.numeric(upd$values))
  expect_identical(length(upd$names), length(upd$values))
  expect_true(all(upd$names %in% prep$cf.names))
  expect_true(all(is.finite(upd$values)))

  # A second outer iteration threads the smoothed_state and still runs; the flat
  # stats length is stable (model-derived, not O(n)).
  prep <- nolimitsEmEStepDS("prep", th, 2L, 20260824L, 0L, 20L)
  st2 <- nolimitsSaemStatsDS("prep", th, 2L)
  expect_identical(length(st2$stats), len1)
  prep <- nolimitsSaemMstepDS("prep", th, .ds_num_encode(st2$stats), 2L, 20L)
  upd2 <- nolimitsSaemUpdatesDS("prep")
  expect_identical(length(upd2$names), length(upd$names))

  # The numerical subset of nolimitsEmQDS: a restricted free-name list.
  num.q1 <- intersect(prep$num.names, prep$q1.names)
  if (length(num.q1)) {
    q <- nolimitsEmQDS("prep", th, "q1", 3L, paste(num.q1, collapse = ","))
    expect_true(q$finite)
    expect_length(q$grad, length(num.q1))
  }
  expect_error(nolimitsEmQDS("prep", th, "q1", 3L, "not_a_param"),
               "free.names must be a subset")
})

test_that("SAEM aggregates fail legibly before any E-step", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  fresh <- list(dm = 1, theta0 = rep(0, 4), names = c("cl", "v", "omega", "sigma"),
                scale = rep(1, 4), p = 4L, n.subjects = 6L, n.obs = 36L,
                model.hash = "x", q1.names = c("cl", "v", "sigma"),
                q2.names = "omega", cf.names = c("omega", "sigma"),
                num.names = c("cl", "v"), em.available = TRUE, versions = list())
  expect_error(nolimitsSaemStatsDS("fresh", .ds_num_encode(rep(0, 4)), 1L),
               "no E-step draws are cached")
  expect_error(nolimitsSaemUpdatesDS("fresh"),
               "no closed-form update is cached")
})
