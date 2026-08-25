# The DP release path against the real fixture model: the coordinate contract,
# the leave-one-subject-out clip bound, the refusals and the ledger enforcement
# as the analyst meets it.

nlds_dp_options <- function(dir, ...) {
  c(nlds_fit_options(),
    list(dsNoLimits.dp.ledgerDir = dir,
         default.dsNoLimits.dp.ledgerDir = dir,
         dsNoLimits.dp.clip = 20,
         dsNoLimits.dp.clipMode = "per-group",
         dsNoLimits.dp.noiseMultiplier = 1,
         dsNoLimits.dp.epsilonBudget = 1e6,
         dsNoLimits.dp.delta = 1e-5,
         dsNoLimits.dp.maxT = 200),
    list(...))
}

test_that("the clipped sum is the preconditioned gradient when nothing binds", {
  nlds_skip_no_julia()
  withr::local_options(nlds_dp_options(withr::local_tempdir()))
  P <- nlds_get_prep()
  theta <- P$theta0
  # THE COORDINATE CONTRACT. The site clips - and therefore noises - the vector
  # the client's optimiser steps with, which is s * grad on the transformed
  # scale. With the clip made non-binding the sum must reproduce it exactly, so
  # the client must NOT multiply the DP gradient by s a second time.
  summed <- .dp_clipped_sum(P, theta, "laplace", 1L, 1e12, "joint")
  ref <- nolimitsObjGradDS("P", .ds_num_encode(theta), "laplace", 1, 1)
  expect_equal(summed, P$scale * ref$grad, tolerance = 1e-8)
  # Per-group mode splits the same vector, so a non-binding clip agrees too.
  expect_equal(.dp_clipped_sum(P, theta, "laplace", 1L, 1e12, "per-group"),
               P$scale * ref$grad, tolerance = 1e-8)
})

test_that("dropping one subject moves the payload by at most the clip bound", {
  nlds_skip_no_julia()
  withr::local_options(nlds_dp_options(withr::local_tempdir()))
  P <- nlds_get_prep()
  g <- .dp_subject_gradients(P, P$theta0, "laplace", 1L)
  expect_equal(nrow(g), as.integer(P$n.subjects))

  worst <- 0
  for (mode in c("joint", "per-group")) {
    spec <- .dp_clip_spec(as.character(P$names), 20, mode)
    full <- .dp_clip_sum(g, spec$ids, spec$clips)
    for (i in seq_len(nrow(g))) {
      loo <- .dp_clip_sum(g[-i, , drop = FALSE], spec$ids, spec$clips)
      delta <- sqrt(sum((full - loo)^2))
      expect_lte(delta, spec$total + 1e-9)
      worst <- max(worst, delta / spec$total)
    }
  }
  # The bound is not vacuous on this fixture: at least one subject actually
  # reaches an appreciable fraction of it.
  expect_gt(worst, 0.01)
  cat(sprintf("\n[dp] worst leave-one-subject-out delta = %.4f of C_total\n",
              worst))
})

test_that("the DP method releases only what the accounting covers", {
  nlds_skip_no_julia()
  dir <- withr::local_tempdir()
  withr::local_options(nlds_dp_options(dir))
  P <- nlds_get_prep()
  enc <- .ds_num_encode(P$theta0)
  res <- nolimitsObjGradDpDS("P", enc, "laplace", 1, 1)
  expect_setequal(names(res), c("grad", "releases", "epsilon",
                                "remaining.budget", "delta", "n.subjects"))
  # No objective value, no natural vector, nothing whose size depends on n.
  expect_length(res$grad, as.integer(P$p))
  expect_true(all(is.finite(res$grad)))
  expect_equal(res$releases, 1L)
  expect_equal(res$epsilon, .dp_epsilon(1, 1e-5), tolerance = 1e-12)
  expect_identical(unserialize(serialize(res, NULL)), res)

  # Fresh noise every call: two identical requests differ.
  again <- nolimitsObjGradDpDS("P", enc, "laplace", 1, 2)
  expect_false(identical(res$grad, again$grad))
  expect_equal(again$releases, 2L)
  expect_gt(again$epsilon, res$epsilon)
  expect_lt(again$remaining.budget, res$remaining.budget)

  # The noised release is centred on the clipped sum: over many draws the mean
  # error is within a few noise standard deviations of zero.
  clipped <- .dp_clipped_sum(P, P$theta0, "laplace", 1L, 20, "per-group")
  spec <- .dp_clip_spec(as.character(P$names), 20, "per-group")
  # A plain loop, not vapply: a *DS function reads parent.frame(), which inside
  # a vapply callback is the callback's own frame, where `P` does not exist.
  draws <- matrix(NA_real_, as.integer(P$p), 20L)
  for (k in seq_len(20L)) {
    draws[, k] <- nolimitsObjGradDpDS("P", enc, "laplace", 1, k + 2)$grad
  }
  expect_lt(max(abs(rowMeans(draws) - clipped)), 3 * spec$total / sqrt(20))
})

test_that("the DP method refuses pooled, map and an unknown estimator", {
  nlds_skip_no_julia()
  withr::local_options(nlds_dp_options(withr::local_tempdir()))
  P <- nlds_get_prep()
  enc <- .ds_num_encode(P$theta0)
  for (est in c("pooled", "map")) {
    expect_error(nolimitsObjGradDpDS("P", enc, est, 1, 1),
                 "no per-subject term")
  }
  expect_error(nolimitsObjGradDpDS("P", enc, "banana", 1, 1),
               "estimator must be one of laplace, focei, ghq, mle")
  # ... and refuses before any release is charged.
  expect_length(list.files(getOption("dsNoLimits.dp.ledgerDir")), 0L)
})

test_that("the DP method validates its arguments and the round cap", {
  nlds_skip_no_julia()
  withr::local_options(nlds_dp_options(withr::local_tempdir(),
                                       dsNoLimits.dp.maxT = 5))
  P <- nlds_get_prep()
  enc <- .ds_num_encode(P$theta0)
  expect_error(nolimitsObjGradDpDS("P", 1, "laplace", 1, 1),
               "theta must be a single character string")
  expect_error(nolimitsObjGradDpDS("P", .ds_num_encode(c(1, 2)), "laplace", 1, 1),
               "must decode to 4 numeric values")
  expect_error(nolimitsObjGradDpDS("P", enc, "ghq", 12, 1),
               "ghq.level must be an integer")
  expect_error(nolimitsObjGradDpDS("P", enc, "laplace", 1, 6),
               "round must be an integer between 1 and 5")
  expect_error(nolimitsObjGradDpDS("no.such.symbol", enc, "laplace", 1, 1),
               "does not exist on this server")
})

test_that("the DP method refuses when the server is not provisioned for it", {
  nlds_skip_no_julia()
  withr::local_options(c(nlds_fit_options(),
                         list(dsNoLimits.dp.ledgerDir = NULL,
                              default.dsNoLimits.dp.ledgerDir = "")))
  P <- nlds_get_prep()
  expect_error(nolimitsObjGradDpDS("P", .ds_num_encode(P$theta0), "laplace", 1, 1),
               "not provisioned for differentially private")
})

test_that("the budget is enforced across sessions and cannot be reset", {
  nlds_skip_no_julia()
  dir <- withr::local_tempdir()
  # A budget of exactly one release: the first is charged, the second refused.
  withr::local_options(nlds_dp_options(
    dir, dsNoLimits.dp.epsilonBudget = .dp_epsilon(1, 1e-5)))
  P <- nlds_get_prep()
  enc <- .ds_num_encode(P$theta0)
  first <- nolimitsObjGradDpDS("P", enc, "laplace", 1, 1)
  expect_equal(first$remaining.budget, 0)
  expect_error(nolimitsObjGradDpDS("P", enc, "laplace", 1, 2),
               "budget for this data set is exhausted")
  # The refusal names no data value and carries no payload.
  trapped <- tryCatch(nolimitsObjGradDpDS("P", enc, "laplace", 1, 3),
                      error = function(e) e)
  expect_s3_class(trapped, "error")
  expect_null(trapped$value)
  expect_null(trapped$grad)
  # A "new session" is just another call: the ledger is on disk, so raising the
  # budget option is the ONLY way back in, and that is the data owner's key.
  withr::with_options(list(dsNoLimits.dp.epsilonBudget = 1e6), {
    expect_equal(nolimitsObjGradDpDS("P", enc, "laplace", 1, 4)$releases, 2L)
  })
})

test_that("the fingerprint of a prep cache follows the data, not the symbol", {
  nlds_skip_no_julia()
  withr::local_options(nlds_dp_options(withr::local_tempdir()))
  P <- nlds_get_prep()
  expect_type(P$fingerprint, "character")
  expect_equal(nchar(P$fingerprint), 64L)
  expect_identical(P$fingerprint,
                   .dp_fingerprint(nlds_test_data(), NLDS_TEST_MODEL))
})

test_that("the mle arm clips per individual and the no-RE prep works", {
  nlds_skip_no_julia()
  dir <- withr::local_tempdir()
  withr::local_options(nlds_dp_options(dir))
  P <- nlds_get_prep_nore()
  g <- .dp_subject_gradients(P, P$theta0, "mle", 1L)
  expect_equal(dim(g), c(as.integer(P$n.subjects), as.integer(P$p)))
  ref <- nolimitsObjGradDS("P", .ds_num_encode(P$theta0), "mle", 1, 1)
  expect_equal(colSums(g), P$scale * ref$grad, tolerance = 1e-8)
  res <- nolimitsObjGradDpDS("P", .ds_num_encode(P$theta0), "mle", 1, 1)
  expect_length(res$grad, as.integer(P$p))
})

test_that("a non-finite per-subject gradient still charges budget and releases", {
  nlds_skip_no_julia()
  dir <- withr::local_tempdir()
  withr::local_options(nlds_dp_options(dir))
  P <- nlds_get_prep()
  # Transformed sigma = 300 (theta[4]) drives every subject's per-subject
  # gradient non-finite WITHOUT throwing - the numerical-instability probe a free
  # error would have handed the analyst for nothing.
  theta <- P$theta0
  theta[4L] <- 300
  enc <- .ds_num_encode(theta)

  # Precondition: at this theta the RAW per-subject gradient really is non-finite,
  # so this test exercises the write-ahead-on-instability path and not a benign one.
  method <- .nlds_fun("nlds_method")("laplace", 1L)
  raw <- as.numeric(.nlds_fun("nlds_dp_batches")(P$dm, theta, method))
  expect_true(any(!is.finite(raw[-(1:2)])))

  before <- length(.dp_ledger_sigmas(.dp_ledger_path(dir, P$fingerprint)))
  res <- nolimitsObjGradDpDS("P", enc, "laplace", 1, 1)
  # (a) a finite noised release, not an error, despite the non-finite gradients.
  expect_length(res$grad, as.integer(P$p))
  expect_true(all(is.finite(res$grad)))
  # (b) the ledger grew: the query cost budget, as every well-formed query must.
  after <- length(.dp_ledger_sigmas(.dp_ledger_path(dir, P$fingerprint)))
  expect_equal(after, before + 1L)
  expect_equal(res$releases, before + 1L)
  expect_gt(res$epsilon, 0)
})

test_that("a clipping unit larger than one subject is refused", {
  nlds_skip_no_julia()
  withr::local_options(nlds_dp_options(withr::local_tempdir()))
  P <- nlds_get_prep()
  # The fixture's single ID-grouped random effect makes batch == subject, which
  # is the case the guard lets through. Constructing a model whose random-effect
  # level spans subjects would need a second @Model and its full Julia code
  # generation, so the guard itself is asserted on a cache whose subject count
  # disagrees with the batch count - the same inconsistency, reached cheaply.
  doctored <- P
  doctored$n.subjects <- as.integer(P$n.subjects) + 1L
  expect_error(.dp_subject_gradients(doctored, P$theta0, "laplace", 1L),
               "one independence unit per subject")
})

# --- EM (MCEM/SAEM) DP release path ------------------------------------------
# The DP twins of nolimitsEmQDS / nolimitsSaemStatsDS: they release only the
# per-subject-clipped, noised aggregate (no Q value, no per-subject data), charge
# the ledger, refuse when unprovisioned or over budget, and bound the
# add/remove-one-subject delta by the clip.

# An E-step must have run this outer iteration for the DP twins to have draws.
nlds_em_cached <- function(prep) {
  th <- .ds_num_encode(prep$theta0)
  nolimitsEmEStepDS("prep", th, 1L, 20260824L, 0L, 20L)
}

test_that("the EM DP methods release only the noised aggregate and charge the ledger", {
  nlds_skip_no_julia()
  dir <- withr::local_tempdir()
  withr::local_options(nlds_dp_options(dir))
  prep <- nlds_get_prep()
  prep <- nlds_em_cached(prep)
  th <- .ds_num_encode(prep$theta0)

  # MCEM Q DP: a gradient only, no Q value; the accounting fields; length = part.
  res <- nolimitsEmQDpDS("prep", th, "q1", 1L)
  expect_setequal(names(res), c("grad", "releases", "epsilon",
                                "remaining.budget", "delta", "n.subjects"))
  expect_null(res$value)
  expect_length(res$grad, length(prep$q1.names))
  expect_true(all(is.finite(res$grad)))
  expect_equal(res$releases, 1L)
  expect_equal(res$epsilon, .dp_epsilon(1, 1e-5), tolerance = 1e-12)
  expect_identical(unserialize(serialize(res, NULL)), res)
  # Fresh noise every call: two identical requests differ, and the ledger grows.
  again <- nolimitsEmQDpDS("prep", th, "q1", 2L)
  expect_false(identical(res$grad, again$grad))
  expect_equal(again$releases, 2L)
  expect_gt(again$epsilon, res$epsilon)

  # SAEM stats DP: the de-normalized flat stats only, fixed length, noised; the
  # same ledger, so this is the third charge on this data set.
  st <- nolimitsSaemStatsDpDS("prep", th, 3L)
  expect_setequal(names(st), c("stats", "releases", "epsilon",
                               "remaining.budget", "delta", "n.subjects"))
  expect_true(all(is.finite(st$stats)))
  expect_length(st$stats, length(as.numeric(
    .nlds_fun("nlds_saem_stats_flat")(prep$dm, prep$theta0, prep$em))))
  expect_equal(st$releases, 3L)
})

test_that("EM DP per-subject clipping bounds the add/remove-one-subject delta", {
  nlds_skip_no_julia()
  dir <- withr::local_tempdir()
  withr::local_options(nlds_dp_options(dir))
  prep <- nlds_get_prep()
  prep <- nlds_em_cached(prep)
  clip <- 20

  # MCEM q1 rows, preconditioned exactly as the release clips them.
  flat <- as.numeric(.nlds_fun("nlds_mcem_dp_rows")(
    prep$dm, prep$theta0, prep$em, "q1", prep$q1.names))
  rows <- .dp_flat_matrix(flat, "rows")
  ss <- as.numeric(prep$scale)[match(prep$q1.names, as.character(prep$names))]
  rows <- rows * rep(ss, each = nrow(rows))
  expect_equal(nrow(rows), as.integer(prep$n.subjects))

  # SAEM stats rows (no preconditioning).
  srows <- .dp_flat_matrix(as.numeric(.nlds_fun("nlds_saem_stats_dp_rows")(
    prep$dm, prep$theta0, prep$em)), "rows")
  expect_equal(nrow(srows), as.integer(prep$n.subjects))

  for (mat in list(rows, srows)) {
    ids <- rep(1L, ncol(mat))
    full <- .dp_clip_sum(mat, ids, clip)
    for (i in seq_len(nrow(mat))) {
      loo <- .dp_clip_sum(mat[-i, , drop = FALSE], ids, clip)
      expect_lte(sqrt(sum((full - loo)^2)), clip + 1e-9)
    }
  }
})

test_that("the EM DP methods refuse when unprovisioned", {
  nlds_skip_no_julia()
  withr::local_options(c(nlds_fit_options(),
                         list(dsNoLimits.dp.ledgerDir = NULL,
                              default.dsNoLimits.dp.ledgerDir = "")))
  prep <- nlds_get_prep()
  prep <- nlds_em_cached(prep)
  th <- .ds_num_encode(prep$theta0)
  expect_error(nolimitsEmQDpDS("prep", th, "q1", 1L),
               "not provisioned for differentially private")
  expect_error(nolimitsSaemStatsDpDS("prep", th, 1L),
               "not provisioned for differentially private")
})

test_that("the EM DP budget is enforced and never overshoots", {
  nlds_skip_no_julia()
  dir <- withr::local_tempdir()
  withr::local_options(nlds_dp_options(
    dir, dsNoLimits.dp.epsilonBudget = .dp_epsilon(1, 1e-5)))
  prep <- nlds_get_prep()
  prep <- nlds_em_cached(prep)
  th <- .ds_num_encode(prep$theta0)
  first <- nolimitsEmQDpDS("prep", th, "q1", 1L)
  expect_equal(first$remaining.budget, 0)
  expect_error(nolimitsEmQDpDS("prep", th, "q1", 2L),
               "budget for this data set is exhausted")
  expect_error(nolimitsSaemStatsDpDS("prep", th, 3L),
               "budget for this data set is exhausted")
})
